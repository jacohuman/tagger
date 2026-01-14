#' Build parent vocab at a specific hierarchy level
#'
#' @param tag_matrix Tag matrix with tag_level_* columns.
#' @param level Level number to build parent vocab for.
#'
#' @return Tibble with parent/child counts for the level.
#' @importFrom rlang .data
#' @export
build_parent_vocab_for_level <- function(tag_matrix, level) {
  stopifnot(level >= 1)
  child_col <- paste0("tag_level_", level)
  parent_col <- paste0("tag_level_", level + 1)

  tag_matrix |>
    dplyr::count(
      parent_tag = .data[[parent_col]],
      tag = .data[[child_col]],
      name = "n"
    ) |>
    dplyr::mutate(level_num = level) |>
    dplyr::relocate(level_num, parent_tag, tag, n)
}

#' Build parent vocab for multiple levels
#'
#' @param tag_matrix Tag matrix with tag_level_* columns.
#' @param levels Integer vector of levels to include.
#'
#' @return Tibble with parent/child counts across levels.
#' @export
build_parent_vocab <- function(tag_matrix, levels = 1:6) {
  purrr::map_dfr(levels, ~ build_parent_vocab_for_level(tag_matrix, .x))
}

#' Clean a parent tag group with an LLM
#'
#' @param vocab_group Tibble of child tags and counts for a single parent tag.
#' @param model Ollama model to use.
#' @param base_url Base URL for the Ollama server.
#' @param num_predict Maximum tokens for the response.
#' @param timeout_sec Request timeout.
#'
#' @return Mapping tibble with canonical tags.
#' @export
llm_clean_parent_group <- function(
    vocab_group,
    model,
    base_url,
    num_predict = 512,
    timeout_sec = 300
) {
  level_num <- unique(vocab_group$level_num)
  parent_tag <- unique(vocab_group$parent_tag)
  stopifnot(length(level_num) == 1, length(parent_tag) == 1)

  tag_lines <- paste0(
    "- ", vocab_group$tag, " (", vocab_group$n, ")",
    collapse = "\n"
  )

  prompt <- paste0(
    "You are cleaning an ontology vocabulary for survey questions. The ultimate goal is to extract an ontology from this tag hierarchy.\n",
    "All tags below are child tags that belong under the SAME parent tag.\n\n",
    "Hierarchy context:\n",
    "- level: ", level_num, "\n",
    "- parent tag: ", parent_tag, "\n\n",
    "Your tasks:\n",
    "1. Group tags that are minor spelling variants or obvious synonyms.\n",
    "2. For each group, choose ONE canonical child tag.\n",
    "3. Canonical name rules: 1-2 English words, lowercase, ASCII letters or underscores only.\n",
    "4. Without inventing new concepts; normalise/merge similar ones under a ontology level tag (i.e. if the child tags are too niche), while keeping it less general than the parent tag.\n",
    "5. If child tags cannot be semantically grouped, don't force groupings.\n\n",
    "IMPORTANT OUTPUT RULES (must follow):\n",
    "- Output ONLY valid JSON.\n",
    "- Do NOT write any explanation or markdown.\n",
    "- Do NOT use code fences.\n",
    "- Your first character MUST be '{' and your last character MUST be '}'.\n\n",
    "Return JSON exactly in this form:\n",
    "{ \"groups\": [\n",
    "  { \"canonical\": \"tag_name\", \"members\": [\"old1\", \"old2\"] },\n",
    "  { \"canonical\": \"another_tag\", \"members\": [\"old3\"] }\n",
    "]}\n\n",
    "Child tags under parent tag '", parent_tag, "' with frequencies:\n",
    tag_lines, "\n"
  )

  raw <- ollama_generate(
    prompt = prompt,
    model = model,
    base_url = base_url,
    num_predict = num_predict,
    temperature = 0.1,
    format = "json",
    timeout_sec = timeout_sec
  )

  json_candidate <- stringr::str_extract(raw, "\\{[\\s\\S]*\\}")
  if (is.na(json_candidate)) {
    warning("No JSON found in response for level ", level_num,
            ", parent ", parent_tag, "; falling back to identity mapping.")
    return(vocab_group |>
      dplyr::transmute(
        level_num = level_num,
        old_tag = tag,
        canonical_tag = tag
      ))
  }

  inner <- tryCatch(
    jsonlite::fromJSON(json_candidate, simplifyVector = FALSE),
    error = function(e) {
      warning("Failed to parse JSON for level ", level_num,
              ", parent ", parent_tag, ": ", conditionMessage(e),
              "; falling back to identity mapping.")
      NULL
    }
  )

  if (is.null(inner) || is.null(inner$groups)) {
    warning("Inner JSON has no 'groups' field for level ", level_num,
            ", parent ", parent_tag, "; falling back to identity mapping.")
    return(vocab_group |>
      dplyr::transmute(
        level_num = level_num,
        old_tag = tag,
        canonical_tag = tag
      ))
  }

  groups <- inner$groups
  if (!is.list(groups) || (!is.null(groups$canonical) && !is.null(groups$members))) {
    groups <- list(groups)
  }

  purrr::map_dfr(groups, function(g) {
    tibble::tibble(
      level_num = level_num,
      old_tag = unlist(g$members, use.names = FALSE),
      canonical_tag = g$canonical
    )
  })
}

#' Build tag mapping using LLM cleanup per parent tag
#'
#' @param parent_vocab Parent vocab tibble from [build_parent_vocab()].
#' @param model Ollama model to use.
#' @param base_url Base URL for the Ollama server.
#' @param num_predict Maximum tokens for the response.
#' @param timeout_sec Request timeout.
#'
#' @return Mapping tibble with canonical tags.
#' @export
build_tag_mapping_with_llm_parent <- function(
    parent_vocab,
    model,
    base_url,
    num_predict = 512,
    timeout_sec = 300
) {
  parent_vocab |>
    dplyr::group_by(level_num, parent_tag) |>
    dplyr::group_split() |>
    purrr::map_dfr(function(vg) {
      llm_clean_parent_group(
        vg,
        model = model,
        base_url = base_url,
        num_predict = num_predict,
        timeout_sec = timeout_sec
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::distinct(level_num, old_tag, canonical_tag)
}

#' Apply tag mapping to a tag matrix
#'
#' @param tag_matrix Tag matrix with tag_level_* columns.
#' @param mapping Mapping tibble from [build_tag_mapping_with_llm_parent()].
#'
#' @return Updated tag matrix with canonical tags applied.
#' @export
apply_tag_mapping <- function(tag_matrix, mapping) {
  cleaned <- tag_matrix
  levels <- sort(unique(mapping$level_num))
  for (level in levels) {
    level_col <- paste0("tag_level_", level)
    replacements <- mapping[mapping$level_num == level, , drop = FALSE]
    cleaned[[level_col]] <- dplyr::coalesce(
      replacements$canonical_tag[match(cleaned[[level_col]], replacements$old_tag)],
      cleaned[[level_col]]
    )
  }
  cleaned
}

#' Clean tag hierarchy
#'
#' @param tag_matrix Tag matrix with tag_level_* columns.
#' @param levels Levels to clean via LLM mapping.
#' @param model Ollama model to use for cleanup.
#' @param base_url Base URL for the Ollama server.
#' @param use_llm Logical, whether to call the LLM to normalize tags.
#'
#' @return List with cleaned tag matrix and optional mapping.
#' @export
clean_tag_hierarchy <- function(
    tag_matrix,
    levels = 1:6,
    model = NULL,
    base_url = NULL,
    use_llm = FALSE
) {
  cleaned <- tag_matrix
  tag_cols <- grep("^tag_level_", names(cleaned), value = TRUE)
  for (i in seq_len(length(tag_cols) - 1)) {
    child_col <- tag_cols[[i]]
    parent_col <- tag_cols[[i + 1]]
    duplicates <- !is.na(cleaned[[child_col]]) &
      cleaned[[child_col]] == cleaned[[parent_col]]
    cleaned[[parent_col]][duplicates] <- NA_character_
  }

  mapping <- NULL
  if (use_llm) {
    stopifnot(!is.null(model), !is.null(base_url))
    level_nums <- sort(as.integer(stringr::str_remove(tag_cols, "tag_level_")))
    max_level <- max(level_nums)
    valid_levels <- levels[levels %in% level_nums & levels < max_level]
    parent_vocab <- build_parent_vocab(cleaned, levels = valid_levels)
    parent_vocab_nontrivial <- parent_vocab |>
      dplyr::group_by(level_num, parent_tag) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()
    mapping <- build_tag_mapping_with_llm_parent(
      parent_vocab_nontrivial,
      model = model,
      base_url = base_url
    )
    cleaned <- apply_tag_mapping(cleaned, mapping)
  }

  list(cleaned_matrix = cleaned, mapping = mapping)
}
