

#' Build and tag hierarchical clusters of survey questions
#'
#' @description
#' This pipeline flattens question texts from a forms tibble, embeds them with
#' the configured Ollama embedding model, performs hierarchical clustering at
#' user-specified levels, and tags clusters bottom-up using an Ollama chat
#' model. Progress is persisted to disk after every cluster tag so that a failed
#' run can be resumed without losing previously generated labels.
#'
#' @param forms Tibble with a list-column \code{form} that contains a
#'   \code{caption} field. See [unnest_questions()].
#' @param clusters_by_level Integer vector giving the desired number of
#'   clusters at each level of the hierarchy, ordered from the most granular
#'   level to the top. The final entry should typically be \code{2} to satisfy
#'   the "two main categories" requirement.
#' @param limit_n Maximum number of unique questions to consider. Passed to
#'   [unnest_questions()].
#' @param sample_size Number of sample questions to pass to the tagger for each
#'   cluster.
#' @param progress_path File path where tagging progress should be stored as an
#'   RDS file. When the file exists it is read and used to resume tagging.
#' @param embed_model Name of the Ollama embedding model.
#' @param tag_model Name of the Ollama chat model used for labeling.
#' @param base Base URL for the Ollama server,
#'   e.g. \code{"http://localhost:11434"}.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{questions} — tibble of question ids, captions, and
#'       embeddings
#'     \item \code{assignments} — question-to-cluster assignments for each
#'       level
#'     \item \code{clusters} — per-cluster metadata and generated tags
#'   }
#'
#' @examples
#' \dontrun{
#' tagged <- run_question_tagger(forms, clusters_by_level = c(12, 6, 2))
#' tagged$clusters
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr map_dfr
#' @importFrom stats dist hclust cutree
#' @importFrom utils head
#' @importFrom rlang .data
#' @export
run_question_tagger <- function(
    forms,
    clusters_by_level,
    limit_n = Inf,
    sample_size = 5,
    progress_path = "question_tagger_state.rds",
    embed_model = EMBED_MODEL,
    tag_model = TAGGER_MODEL,
    base = OLLAMA_BASE
) {
  if (file.exists(progress_path)) {
    message("Resuming tagging from existing progress file: ", progress_path)
    state <- readRDS(progress_path)
    if (is.null(state$assignments)) {
      stop("Progress file is missing assignments; remove it to restart tagging.")
    }
  } else {
    message("No progress file found; starting fresh hierarchy build...")
    hierarchy <- build_question_hierarchy(
      forms = forms,
      clusters_by_level = clusters_by_level,
      limit_n = limit_n,
      embed_model = embed_model,
      base = base
    )

    # hierarchy$questions is your embedded questions dataframe (step 2)
    # hierarchy$assignments holds cluster ids per level (step 4)

    clusters <- build_cluster_index(hierarchy$assignments, clusters_by_level)
    state <- list(
      questions = hierarchy$questions,
      assignments = hierarchy$assignments,
      clusters = clusters,
      clusters_by_level = clusters_by_level
    )
    saveRDS(state, progress_path)
    message("Initial state saved to: ", progress_path)
  }

  state$clusters <- tag_clusters_bottom_up(
    clusters = state$clusters,
    questions = state$questions,
    assignments = state$assignments,
    sample_size = sample_size,
    progress_path = progress_path,
    model = tag_model,
    base = base
  )

  # Returned list:
  # - state$questions   : id, caption, embedding  (your step 2)
  # - state$assignments : question + cluster_level_* columns (step 4)
  # - state$clusters    : one row per cluster with level, parent_cluster, tag, question_ids

  state
}


#' Tag clusters from the bottom of the hierarchy upward
#'
#' @description
#' Iterates through each level, generating tags for clusters whose tags are not
#' yet set. After each successful tag, the updated state is written to
#' \code{progress_path}.
#'
#' @param clusters Tibble produced by [build_cluster_index()].
#' @param questions Question tibble with \code{id} and \code{caption} columns.
#' @param assignments Question-to-cluster assignments. Persisted so an interrupted
#'   run can resume with full context.
#' @param sample_size Number of example questions to include in the prompt.
#' @param progress_path File path where the full tagging state list is stored.
#' @param model Ollama chat model for tagging.
#' @param base Base URL for the Ollama server.
#'
#' @return The clusters tibble with the \code{tag} column filled.
#' @keywords internal
tag_clusters_bottom_up <- function(
    clusters,
    questions,
    assignments,
    sample_size,
    progress_path,
    model,
    base) {

  max_level <- max(clusters$level)

  for (level_idx in seq_len(max_level)) {
    message("\n---- Tagging level ", level_idx, " of ", max_level, " ----")
    level_clusters <- clusters[clusters$level == level_idx, , drop = FALSE]

    for (row_idx in seq_len(nrow(level_clusters))) {
      cluster_row <- level_clusters[row_idx, ]
      # Only skip clusters that already have a *real* tag.
      # Treat "untagged" as unfinished so we retry it on resume.
      if (!is.na(cluster_row$tag) &&
          nzchar(cluster_row$tag) &&
          cluster_row$tag != "untagged") {
        next
      }

      q_ids <- cluster_row$question_ids[[1]]
      q_caps <- questions$caption[match(q_ids, questions$id)]
      q_caps <- unique(na.omit(q_caps))

      # Representative sample for this cluster (your step 3 / 4)
      if (length(q_caps) > sample_size) {
        set.seed(42)  # deterministic sampling if you like
        examples <- sample(q_caps, sample_size)
      } else {
        examples <- q_caps
      }

      existing <- clusters$tag[
        !is.na(clusters$tag) &
          nzchar(clusters$tag) &
          clusters$tag != "untagged"
      ]

      if (level_idx == 1L) {
        # Deepest level: tag small clusters directly
        tag <- tag_one_cluster_ollama(
          captions = examples,
          cluster_id = cluster_row$cluster_id,
          model = model,
          base = base
        )
      } else {
        # Parent levels: roll up from children
        children <- clusters[
          clusters$parent_cluster == cluster_row$cluster_id &
            clusters$level == level_idx - 1L,
          ,
          drop = FALSE
        ]

        child_details <- purrr::map(seq_len(nrow(children)), function(i) {
          child_row <- children[i, ]
          q_ids_child <- child_row$question_ids[[1]]
          q_caps_child <- questions$caption[match(q_ids_child, questions$id)]
          list(
            cluster_id = child_row$cluster_id,
            tag = child_row$tag,
            samples = utils::head(unique(na.omit(q_caps_child)), sample_size)
          )
        })

        tag <- tag_parent_cluster(
          child_clusters = child_details,
          parent_examples = examples,
          cluster_id = cluster_row$cluster_id,
          model = model,
          base = base
        )
      }

      final_tag <- if (identical(tag, "untagged")) {
        "untagged"
      } else {
        tag
      }

      clusters$tag[
        clusters$level == level_idx &
          clusters$cluster_id == cluster_row$cluster_id
      ] <- final_tag

      # Persist full state after each cluster (your resume requirement)
      state <- list(
        questions = questions,
        assignments = assignments,
        clusters = clusters
      )
      saveRDS(state, progress_path)
      message(
        "Saved progress after level ", level_idx,
        ", cluster ", cluster_row$cluster_id,
        " -> tag '", final_tag, "'."
      )

      # If this tag is untagged, stop so you can investigate / resume
      if (identical(final_tag, "untagged")) {
        stop(
          sprintf(
            "Tagging halted after cluster %s at level %s was marked 'untagged'. Progress saved to %s.",
            cluster_row$cluster_id, level_idx, progress_path
          )
        )
      }
    }
  }

  clusters
}

#' Tag a parent cluster based on its child tags and sample questions
#'
#' @param child_clusters List of child cluster metadata including tag and sample
#'   questions.
#' @param parent_examples Sample questions belonging to the parent cluster.
#' @inheritParams tag_one_cluster_ollama
#'
#' @return A character scalar label for the parent cluster.
#' @keywords internal
tag_parent_cluster <- function(
    child_clusters,
    parent_examples,
    cluster_id,
    model = TAGGER_MODEL,
    base = OLLAMA_BASE) {

  child_lines <- purrr::map_chr(child_clusters, function(child) {
    tag <- child$tag %||% "untagged"
    samples <- paste(child$samples, collapse = " | ")
    paste0("- child cluster ", child$cluster_id,
           " (tag: ", tag, "): ", samples)
  })

  prompt <- paste(
    "You are building a hierarchical label tree for survey questions.",
    "",
    "Below are several CHILD CLUSTERS and the current tags for each.",
    "Your job is to create ONE PARENT LABEL that groups all of them.",
    "",
    "OUTPUT FORMAT:",
    "- Output ONE SHORT LABEL (exactly one word), lowercase, ASCII letters only.",
    "- No punctuation, no spaces, no quotes.",
    "- No explanation.",
    "",
    "REQUIREMENTS:",
    "- The parent label must be STRICTLY MORE GENERAL than EACH child tag.",
    "- It must still capture a clear, coherent theme shared by all child clusters.",
    "- Do NOT return any of the child tags unchanged, unless it is absolutely impossible",
    "  to find a broader, meaningful word.",
    "- Avoid very broad, generic words like 'survey', 'questions', 'general',",
    "  'other', 'miscellaneous', 'unknown', 'unclear'.",
    "- Compared to the leaf-level tags, this label should be MORE GENERAL, not more specific.",
    "",
    "Child clusters (with tags and example questions):",
    paste(child_lines, collapse = "\n"),
    "",
    paste0("Parent cluster id: ", cluster_id),
    sep = "\n"
  )

  # ---- Debug: print prompt ----
  message("\n==============================")
  message("tag_parent_cluster() – parent cluster ", cluster_id)
  message("Prompt:\n", prompt)

  req <- httr2::request(paste0(base, "/api/generate")) |>
    httr2::req_body_json(list(
      model  = model,
      prompt = prompt,
      stream = FALSE,
      options = list(temperature = 0.1, num_predict = 10)
    )) |>
    httr2::req_timeout(180)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    message("Error calling Ollama (parent): ", conditionMessage(resp))
    return("untagged")
  }

  j <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE),
                error = function(e) e)
  if (inherits(j, "error")) {
    message("Error parsing JSON (parent): ", conditionMessage(j))
    return("untagged")
  }

  raw <- tryCatch({
    if (!is.null(j$response)) {
      j$response
    } else if (!is.null(j$message$content)) {
      j$message$content
    } else if (!is.null(j$choices) && length(j$choices) > 0 &&
               !is.null(j$choices[[1]]$message$content)) {
      j$choices[[1]]$message$content
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)

  message("Raw parent model output: ", raw)

  lab <- sanitize_label(raw)

  if (is.null(lab) || is.na(lab) || nchar(lab) < 2L) {
    message("Parent label unusable, marking as 'untagged'.")
    lab <- "untagged"
  }

  lab
}

