

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
#'   [unnest_questions()]. This is used for the sake of testing so that the pipeline runs quicker.
#' @param sample_size Number of sample questions to pass to the tagger for each
#'   cluster. The larger the sample size, the bigger the prompt and the longer the model will take to respond.
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
#'
#' @export
run_question_tagger <- function(
    forms,
    clusters_by_level,
    limit_n = Inf,
    sample_size = 5,
    progress_path = "question_tagger_state.rds",
    embed_model = ollama_config()$embed_model,
    tag_model = ollama_config()$tagger_model,
    base_url = ollama_config()$base_url
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
      base_url = base_url
    )

    # hierarchy$questions is the embedded questions dataframe.
    # hierarchy$assignments holds cluster ids per level.

    clusters <- build_cluster_index(hierarchy$assignments, clusters_by_level)

    print("Printing clusters")
    print(clusters)
    state <- list(
      questions = hierarchy$questions,
      assignments = hierarchy$assignments,
      clusters = clusters,
      clusters_by_level = clusters_by_level
    )
    save_tag_state(structure(state, class = "tag_state"), progress_path)
    message("Initial state saved to: ", progress_path)
  }

  if (!inherits(state, "tag_state")) {
    state <- structure(state, class = "tag_state")
  }

  state <- tag_clusters_bottom_up(
    state = state,
    sample_size = sample_size,
    progress_path = progress_path,
    model = tag_model,
    base_url = base_url
  )

  # Returned list:
  # - state$questions   : id, caption, embedding.
  # - state$assignments : question + cluster_level_* columns.
  # - state$clusters    : one row per cluster with level, parent_cluster, tag, question_ids.

  state
}


#' Tag clusters from the bottom of the hierarchy upward
#'
#' @description
#' Iterates through each level, generating tags for clusters whose tags are not
#' yet set. After each successful tag, the updated state is written to
#' \code{progress_path}. The hierarchy is tagged bottom up meaning the leaf nodes
#' are tagged first. Thereafter parent nodes are tagged with each tag getting broader
#' to describe a bigger selection of questions.
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
    state,
    sample_size,
    progress_path,
    model,
    base_url) {
  clusters <- state$clusters
  questions <- state$questions
  assignments <- state$assignments

  max_level <- max(clusters$level)

  for (level_idx in seq_len(max_level)) {
    message("\n---- Tagging level ", level_idx, " of ", max_level, " ----")
    level_clusters <- clusters[clusters$level == level_idx, , drop = FALSE]

    print("level_clusters")
    print(level_clusters)

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

      print("Printing q_ids")
      print(q_ids)
      print("Printing q_caps")
      print(q_caps)

      # Representative sample for this cluster
      if (length(q_caps) > sample_size) {
        # set.seed(42)  deterministic sampling if needed
        examples <- sample(q_caps, sample_size)
      } else {
        examples <- q_caps
      }

      existing <- clusters$tag[
        !is.na(clusters$tag) &
          nzchar(clusters$tag) &
          clusters$tag != "untagged"
      ]

      print("Printing existing")
      print(existing)

      if (level_idx == 1L) {
        # Deepest level: tag small clusters directly
        tag <- tag_one_cluster_ollama(
          captions = examples,
          cluster_id = cluster_row$cluster_id,
          model = model,
          base_url = base_url
        )
      } else {
        # Parent levels: roll up from children
        # Each parent tag is determined by considering information regarding its
        # children.  The two child tags are given as context as well as sample
        # question captions that are contained within each child cluster.
        children <- clusters[
          clusters$parent_cluster == cluster_row$cluster_id &
            clusters$level == level_idx - 1L,
          ,
          drop = FALSE
        ]

        print("Printing children")
        print(children)

        # This function collects info about each of the children used as context
        # within the prompt
        child_details <- purrr::map(seq_len(nrow(children)), function(i) {
          child_row <- children[i, ]

          q_ids_child <- unique(na.omit(child_row$question_ids[[1]]))
          if (length(q_ids_child) > sample_size) {
            sample_ids <- sample(q_ids_child, sample_size)
          } else {
            sample_ids <- q_ids_child
          }

          # Since we're tagging parent at `level_idx`, the child level is `level_idx - 1`
          # Include the full known hierarchy up to the child level.
          formatted_samples <- purrr::map_chr(
            sample_ids,
            ~ format_question_with_path(
              q_id = .x,
              questions = questions,
              assignments = assignments,
              clusters = clusters,
              upto_level = level_idx - 1L
            )
          )

          list(
            cluster_id = child_row$cluster_id,
            tag = child_row$tag,
            samples = formatted_samples
          )
        })

        print("Printing child details")
        print(child_details)

        tag <- tag_parent_cluster(
          child_clusters = child_details,
          parent_examples = examples,
          cluster_id = cluster_row$cluster_id,
          model = model,
          base_url = base_url
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

      # Persist full state after each cluster.
      state$clusters <- clusters
      save_tag_state(state, progress_path)
      message(
        "Saved progress after level ", level_idx,
        ", cluster ", cluster_row$cluster_id,
        " -> tag '", final_tag, "'."
      )

      # If this tag is untagged, stop to investigate / resume later.
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

  state
}

#' Tag a parent cluster based on its child tags and sample questions
#'
#' @param child_clusters List of child cluster metadata including tag and sample
#'   questions.
#' @param parent_examples Sample questions belonging to the parent cluster.
#'
#' @inheritParams tag_one_cluster_ollama
#'
#' @return A character scalar label for the parent cluster.
#' @keywords internal
tag_parent_cluster <- function(
    child_clusters,
    parent_examples,
    cluster_id,
    model,
    base_url) {

  child_lines <- purrr::map_chr(child_clusters, function(child) {
    tag <- rlang::`%||%`(child$tag, "untagged")
    samp <- child$samples
    if (length(samp) == 0) samp <- "(no samples)"

    paste0(
      "- child cluster ", child$cluster_id, " (tag: ", tag, ")\n",
      paste0("  - ", samp, collapse = "\n")
    )
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
    "- Do NOT output any label that appears anywhere in the provided {path: ...} hierarchies.",
    "  (Those are lower-level labels that already exist.)",
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

  raw <- tryCatch(
    ollama_generate(
      prompt = prompt,
      model = model,
      base_url = base_url,
      temperature = 0.1,
      num_predict = 10
    ),
    error = function(e) {
      message("Error calling Ollama (parent): ", conditionMessage(e))
      NA_character_
    }
  )

  message("Raw parent model output: ", raw)

  lab <- sanitize_label(raw)

  if (is.null(lab) || is.na(lab) || nchar(lab) < 2L) {
    message("Parent label unusable, marking as 'untagged'.")
    lab <- "untagged"
  }

  lab
}

#' Tag a single cluster using Ollama
#'
#' @param captions Character vector of sample questions.
#' @param cluster_id Cluster identifier.
#' @param model Ollama model to use.
#' @param base_url Base URL for the Ollama server.
#'
#' @return A short tag label.
#' @keywords internal
tag_one_cluster_ollama <- function(
    captions,
    cluster_id,
    model,
    base_url
) {
  examples <- paste0("- ", captions, collapse = "\n")
  prompt <- paste(
    "You are labeling a cluster of survey questions.",
    "",
    "OUTPUT FORMAT:",
    "- Output ONE SHORT LABEL (exactly one word), lowercase, ASCII letters only.",
    "- No punctuation, no spaces, no quotes.",
    "- No explanation.",
    "",
    "Cluster id:", cluster_id,
    "",
    "Example questions:",
    examples,
    sep = "\n"
  )

  raw <- tryCatch(
    ollama_generate(
      prompt = prompt,
      model = model,
      base_url = base_url,
      temperature = 0.2,
      num_predict = 10
    ),
    error = function(e) {
      message("Error calling Ollama (leaf): ", conditionMessage(e))
      NA_character_
    }
  )

  label <- sanitize_label(raw)
  if (is.null(label) || is.na(label) || nchar(label) < 2L) {
    "untagged"
  } else {
    label
  }
}

#' Sanitize a raw label into a single word
#'
#' @param raw Raw label from the model.
#'
#' @return Sanitized label.
#' @keywords internal
sanitize_label <- function(raw) {
  if (is.null(raw) || is.na(raw)) {
    return(NA_character_)
  }
  cleaned <- stringr::str_squish(tolower(raw))
  stringr::str_extract(cleaned, "^[a-z]+")
}

#' Build a question tag matrix from cluster tags
#'
#' @param assignments Question-to-cluster assignments.
#' @param clusters Cluster tags with level and cluster_id.
#'
#' @return Tag matrix with tag_level_* columns.
#'
#' @examples
#' assignments <- tibble::tibble(
#'   id = c("q_001", "q_002"),
#'   caption = c("Age?", "Income?"),
#'   cluster_level_1 = c(1L, 2L),
#'   cluster_level_2 = c(1L, 1L)
#' )
#' clusters <- tibble::tibble(
#'   level = c(1L, 1L, 2L),
#'   cluster_id = c(1L, 2L, 1L),
#'   tag = c("age", "income", "demographics")
#' )
#' build_question_tag_matrix(assignments, clusters)
#' @export
build_question_tag_matrix <- function(assignments, clusters) {
  tag_lookup <- clusters |>
    dplyr::select(level, cluster_id, tag)

  assignments |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("cluster_level_"),
      names_to = "level_name",
      values_to = "cluster_id"
    ) |>
    dplyr::mutate(
      level = as.integer(stringr::str_remove(level_name, "cluster_level_"))
    ) |>
    dplyr::left_join(tag_lookup, by = c("level", "cluster_id")) |>
    dplyr::select(id, caption, level, tag) |>
    dplyr::mutate(level_name = paste0("tag_level_", level)) |>
    dplyr::select(-level) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(
      names_from = level_name,
      values_from = tag
    ) |>
    dplyr::arrange(id)
}

#' Run the full tagging pipeline with resumable state
#'
#' @param forms_path Path to the forms.Rda file.
#' @param clusters_by_level Integer vector of cluster counts by level.
#' @param limit_n Maximum number of unique questions to include.
#' @param sample_size Number of sample questions per cluster for tagging.
#' @param progress_path File path to persist state.
#' @param config List from [ollama_config()].
#' @param clean_levels Levels to pass to [clean_tag_hierarchy()].
#' @param use_llm_cleaning Logical, whether to use LLM cleanup.
#'
#' @return A `tag_state` object with all pipeline outputs.
#'
#' @examples
#' \dontrun{
#' state <- run_tagger_pipeline(
#'   forms_path = "forms.Rda",
#'   clusters_by_level = c(8, 4, 2),
#'   limit_n = 200
#' )
#' state$tag_matrix
#' }
#' @export
run_tagger_pipeline <- function(
    forms_path,
    clusters_by_level,
    limit_n = Inf,
    sample_size = 5,
    progress_path = "question_tagger_state.rds",
    config = ollama_config(),
    clean_levels = 1:6,
    use_llm_cleaning = FALSE
) {
  if (file.exists(progress_path)) {
    state <- load_tag_state(progress_path)
    if (!inherits(state, "tag_state")) {
      state <- structure(state, class = "tag_state")
    }
  } else {
    forms <- load_forms(forms_path)
    questions <- unnest_questions(forms, limit_n = limit_n)
    state <- new_tag_state(questions)
    save_tag_state(state, progress_path)
  }

  if (is.null(state$embeddings)) {
    embeddings <- embed_multiple_questions(
      texts = state$questions$caption,
      model = config$embed_model,
      base_url = config$base_url
    )
    state$questions$embedding <- embeddings
    state$embeddings <- do.call(rbind, embeddings)
    save_tag_state(state, progress_path)
  }

  if (is.null(state$hclust) || is.null(state$assignments) || is.null(state$clusters)) {
    clustering <- cluster_embeddings(state$embeddings, clusters_by_level)
    assignments <- add_cluster_assignments(state$questions, clustering$hclust, clusters_by_level)
    clusters <- build_cluster_index(assignments, clusters_by_level)
    state$hclust <- clustering$hclust
    state$assignments <- assignments
    state$clusters <- clusters
    save_tag_state(state, progress_path)
  }

  if (any(is.na(state$clusters$tag)) || any(state$clusters$tag == "untagged")) {
    state <- tag_clusters_bottom_up(
      state = state,
      sample_size = sample_size,
      progress_path = progress_path,
      model = config$tagger_model,
      base_url = config$base_url
    )
  }

  if (is.null(state$tag_matrix)) {
    state$tag_matrix <- build_question_tag_matrix(state$assignments, state$clusters)
    save_tag_state(state, progress_path)
  }

  if (is.null(state$cleaned)) {
    cleaned <- clean_tag_hierarchy(
      state$tag_matrix,
      levels = clean_levels,
      model = config$tagger_model,
      base_url = config$base_url,
      use_llm = use_llm_cleaning
    )
    state$tag_matrix <- cleaned$cleaned_matrix
    state$cleaned <- cleaned$mapping
    save_tag_state(state, progress_path)
  }

  if (is.null(state$audit)) {
    state <- audit_tag_paths(state, allow_manual = FALSE)
    save_tag_state(state, progress_path)
  }

  state
}

# Return vector of cluster_level_* column names in numeric order
cluster_level_cols <- function(assignments) {
  cols <- grep("^cluster_level_[0-9]+$", names(assignments), value = TRUE)
  # order by the numeric suffix
  ord <- order(as.integer(sub("^cluster_level_", "", cols)))
  cols[ord]
}

# Safely get a cluster tag at a given level/id
get_cluster_tag <- function(clusters, level, cluster_id) {
  hit <- clusters$tag[clusters$level == level & clusters$cluster_id == cluster_id]
  hit <- hit[!is.na(hit) & nzchar(hit)]
  if (length(hit) == 0) return("untagged")
  hit[[1]]
}

# Build "tag1 > tag2 > tag3" for a question, up to a level
question_tag_path <- function(q_id, assignments, clusters, upto_level) {
  row_idx <- match(q_id, assignments$id)
  if (is.na(row_idx)) return(character())

  lvl_cols <- cluster_level_cols(assignments)
  # clamp upto_level to available columns
  upto_level <- min(upto_level, length(lvl_cols))
  if (upto_level < 1) return(character())

  path <- character(0)
  for (lvl in seq_len(upto_level)) {
    cid <- assignments[[lvl_cols[[lvl]]]][row_idx]
    if (is.na(cid) || !nzchar(as.character(cid))) {
      path <- c(path, "untagged")
      next
    }
    path <- c(path, get_cluster_tag(clusters, lvl, cid))
  }

  path
}

# Format a sample question with its hierarchy
format_question_with_path <- function(q_id, questions, assignments, clusters, upto_level) {
  cap <- questions$caption[match(q_id, questions$id)]
  cap <- cap[!is.na(cap)]
  if (length(cap) == 0) cap <- ""
  cap <- cap[[1]]

  path <- question_tag_path(q_id, assignments, clusters, upto_level = upto_level)
  if (length(path) == 0) return(cap)

  paste0(cap, "  {path: ", paste(path, collapse = " > "), "}")
}

