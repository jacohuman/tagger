library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

#' Cosine similarity between two numeric vectors
#'
#' @param a Numeric vector.
#' @param b Numeric vector.
#'
#' @return Numeric cosine similarity or NA.
cosine_sim <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA_real_)
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  if (length(a) != length(b)) return(NA_real_)

  d <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (d == 0) return(NA_real_)

  sum(a * b) / d
}

#' Build cumulative prefixes from a tag vector
#'
#' @param tags Character vector of tags in order.
#' @param sep Separator.
#'
#' @return Character vector of cumulative prefixes.
build_prefixes <- function(tags, sep = " > ") {
  tags <- as.character(tags)
  tags <- tags[!is.na(tags)]
  tags <- str_trim(tags)
  tags <- tags[tags != ""]

  if (length(tags) == 0) return(character(0))

  vapply(seq_along(tags), function(i) {
    paste(tags[seq_len(i)], collapse = sep)
  }, character(1))
}

#' Build path string from cleaned tag_level_* columns
#'
#' @param tag_matrix_clean Cleaned tag matrix.
#' @param tag_cols Optional tag columns.
#'
#' @return Tibble with id, caption, after_path.
build_audit_paths_from_clean <- function(tag_matrix_clean, tag_cols = NULL) {
  if (is.null(tag_cols)) {
    tag_cols <- grep("^tag_level_", names(tag_matrix_clean), value = TRUE)
  }
  stopifnot(length(tag_cols) > 0)

  tag_matrix_clean %>%
    dplyr::select(id, caption) %>%
    dplyr::mutate(
      after_path = purrr::pmap_chr(
        dplyr::select(tag_matrix_clean, dplyr::all_of(tag_cols)),
        ~ paste(stats::na.omit(c(...)), collapse = " > ")
      )
    )
}

# ------------------------------------------------------------
# 1) Build validation tables
# ------------------------------------------------------------

#' Build one row per question-tag pair
#'
#' @param audit_df Tibble with id, caption, after_path.
#' @param path_col Path column name.
#'
#' @return Tibble with one row per tag in the path.
build_question_tag_long <- function(audit_df, path_col = "after_path") {
  stopifnot(all(c("id", "caption", path_col) %in% names(audit_df)))

  tmp <- audit_df %>%
    dplyr::transmute(
      id = .data$id,
      caption = .data$caption,
      after_path = .data[[path_col]]
    )

  purrr::pmap_dfr(
    tmp,
    function(id, caption, after_path) {
      tags <- split_path(after_path)
      tags <- canon_tag(tags)
      tags <- tags[!is.na(tags)]
      tags <- tags[tags != ""]

      if (length(tags) == 0) {
        return(tibble::tibble(
          id = character(0),
          caption = character(0),
          after_path = character(0),
          level = integer(0),
          tag = character(0)
        ))
      }

      tibble::tibble(
        id = id,
        caption = caption,
        after_path = after_path,
        level = seq_along(tags),
        tag = tags
      )
    }
  )
}

#' Build one row per question-prefix pair
#'
#' @param audit_df Tibble with id, caption, after_path.
#' @param path_col Path column name.
#'
#' @return Tibble with one row per cumulative prefix.
build_question_prefix_long <- function(audit_df, path_col = "after_path") {
  stopifnot(all(c("id", "caption", path_col) %in% names(audit_df)))

  tmp <- audit_df %>%
    dplyr::transmute(
      id = .data$id,
      caption = .data$caption,
      after_path = .data[[path_col]]
    )

  purrr::pmap_dfr(
    tmp,
    function(id, caption, after_path) {
      tags <- split_path(after_path)
      tags <- canon_tag(tags)
      tags <- tags[!is.na(tags)]
      tags <- tags[tags != ""]

      prefixes <- build_prefixes(tags)

      if (length(prefixes) == 0) {
        return(tibble::tibble(
          id = character(0),
          caption = character(0),
          after_path = character(0),
          level = integer(0),
          prefix = character(0)
        ))
      }

      tibble::tibble(
        id = id,
        caption = caption,
        after_path = after_path,
        level = seq_along(prefixes),
        prefix = prefixes
      )
    }
  )
}

# ------------------------------------------------------------
# 2) Embedding functions
# ------------------------------------------------------------

#' Embed all unique questions by id
#'
#' @param audit_df Tibble with id and caption.
#' @param model Embedding model name.
#' @param base_url Ollama base URL.
#' @param timeout_sec Timeout in seconds.
#' @param progress Print progress.
#'
#' @return Named list keyed by id.
embed_questions <- function(audit_df,
                            model,
                            base_url,
                            timeout_sec = 60,
                            progress = TRUE) {
  qs <- audit_df %>%
    dplyr::distinct(id, caption)

  out <- vector("list", nrow(qs))
  names(out) <- qs$id

  for (i in seq_len(nrow(qs))) {
    if (progress) {
      message(sprintf("[%d/%d] embedding question: %s", i, nrow(qs), qs$id[[i]]))
    }
    out[[qs$id[[i]]]] <- ollama_embed(
      paste0("Survey question: ", qs$caption[[i]]),
      model = model,
      base_url = base_url,
      timeout_sec = timeout_sec
    )
  }

  out
}

#' Embed all unique tags
#'
#' @param question_tag_long Output of build_question_tag_long().
#' @param model Embedding model name.
#' @param base_url Ollama base URL.
#' @param timeout_sec Timeout in seconds.
#' @param progress Print progress.
#'
#' @return Named list keyed by tag.
embed_tags_for_validation <- function(question_tag_long,
                                      model,
                                      base_url,
                                      timeout_sec = 60,
                                      progress = TRUE) {
  tags <- question_tag_long %>%
    dplyr::distinct(tag) %>%
    dplyr::pull(tag)

  out <- vector("list", length(tags))
  names(out) <- tags

  for (i in seq_along(tags)) {
    tg <- tags[[i]]
    if (progress) {
      message(sprintf("[%d/%d] embedding tag: %s", i, length(tags), tg))
    }
    out[[tg]] <- ollama_embed(
      paste0("Survey question tag: ", tg),
      model = model,
      base_url = base_url,
      timeout_sec = timeout_sec
    )
  }

  out
}

#' Embed all unique prefixes
#'
#' @param question_prefix_long Output of build_question_prefix_long().
#' @param model Embedding model name.
#' @param base_url Ollama base URL.
#' @param timeout_sec Timeout in seconds.
#' @param progress Print progress.
#'
#' @return Named list keyed by prefix string.
embed_prefixes_for_validation <- function(question_prefix_long,
                                          model,
                                          base_url,
                                          timeout_sec = 60,
                                          progress = TRUE) {
  prefixes <- question_prefix_long %>%
    dplyr::distinct(prefix) %>%
    dplyr::pull(prefix)

  out <- vector("list", length(prefixes))
  names(out) <- prefixes

  for (i in seq_along(prefixes)) {
    px <- prefixes[[i]]
    if (progress) {
      message(sprintf("[%d/%d] embedding prefix: %s", i, length(prefixes), px))
    }
    out[[px]] <- ollama_embed(
      paste0("Survey question tag path: ", px),
      model = model,
      base_url = base_url,
      timeout_sec = timeout_sec
    )
  }

  out
}

# ------------------------------------------------------------
# 3) Score per-tag applicability
# ------------------------------------------------------------

#' Score question-tag similarity
#'
#' @param question_tag_long Output of build_question_tag_long().
#' @param question_emb Named list keyed by id.
#' @param tag_emb Named list keyed by tag.
#'
#' @return Tibble with tag similarity scores.
score_question_tag_similarity <- function(question_tag_long,
                                          question_emb,
                                          tag_emb) {
  question_tag_long %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tag_sim = cosine_sim(question_emb[[id]], tag_emb[[tag]])
    ) %>%
    dplyr::ungroup()
}

# ------------------------------------------------------------
# 4) Score cumulative prefix progression
# ------------------------------------------------------------

#' Score question-prefix similarity and progression deltas
#'
#' @param question_prefix_long Output of build_question_prefix_long().
#' @param question_emb Named list keyed by id.
#' @param prefix_emb Named list keyed by prefix.
#'
#' @return Tibble with prefix similarity scores and deltas.
score_question_prefix_similarity <- function(question_prefix_long,
                                             question_emb,
                                             prefix_emb) {
  question_prefix_long %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      prefix_sim = cosine_sim(question_emb[[id]], prefix_emb[[prefix]])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id, level) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      prefix_delta = prefix_sim - dplyr::lag(prefix_sim),
      prefix_delta = dplyr::if_else(is.na(prefix_delta), 0, prefix_delta)
    ) %>%
    dplyr::ungroup()
}

# ------------------------------------------------------------
# 5) Combine signals + flag suspicious cases
# ------------------------------------------------------------

#' Combine tag-level and prefix-level validation signals
#'
#' @param tag_scores Output of score_question_tag_similarity().
#' @param prefix_scores Output of score_question_prefix_similarity().
#' @param min_tag_sim Minimum acceptable tag similarity.
#' @param min_prefix_sim Minimum acceptable final prefix similarity.
#' @param max_negative_delta Maximum tolerated negative prefix delta.
#'
#' @return List with pair-level and path-level validation tables.
validate_question_tag_paths <- function(tag_scores,
                                        prefix_scores,
                                        min_tag_sim = 0.30,
                                        min_prefix_sim = 0.40,
                                        max_negative_delta = -0.05) {
  stopifnot(all(c("id", "level", "tag", "tag_sim") %in% names(tag_scores)))
  stopifnot(all(c("id", "level", "prefix", "prefix_sim", "prefix_delta") %in% names(prefix_scores)))

  pair_level <- tag_scores %>%
    dplyr::left_join(
      prefix_scores %>%
        dplyr::select(id, level, prefix, prefix_sim, prefix_delta),
      by = c("id", "level")
    ) %>%
    dplyr::mutate(
      low_tag_sim = is.na(tag_sim) | tag_sim < min_tag_sim,
      negative_prefix_delta = level > 1 & (is.na(prefix_delta) | prefix_delta < max_negative_delta),
      suspicious = low_tag_sim | negative_prefix_delta,
      suspicion_reason = dplyr::case_when(
        low_tag_sim & negative_prefix_delta ~ "low_tag_sim + negative_prefix_delta",
        low_tag_sim ~ "low_tag_sim",
        negative_prefix_delta ~ "negative_prefix_delta",
        TRUE ~ ""
      )
    )

  path_level <- prefix_scores %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      caption = dplyr::first(caption),
      after_path = dplyr::first(after_path),
      n_levels = dplyr::n(),
      final_prefix_sim = prefix_sim[which.max(level)],
      best_prefix_sim = max(prefix_sim, na.rm = TRUE),
      best_prefix_level = level[which.max(prefix_sim)],
      min_prefix_delta = min(prefix_delta[-1], na.rm = TRUE),
      has_negative_step = any(prefix_delta[-1] < max_negative_delta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      final_prefix_low = is.na(final_prefix_sim) | final_prefix_sim < min_prefix_sim,
      final_not_best = best_prefix_level < n_levels,
      suspicious_path = final_prefix_low | has_negative_step | final_not_best,
      path_reason = dplyr::case_when(
        final_prefix_low & has_negative_step & final_not_best ~ "final_prefix_low + negative_step + final_not_best",
        final_prefix_low & has_negative_step ~ "final_prefix_low + negative_step",
        final_prefix_low & final_not_best ~ "final_prefix_low + final_not_best",
        has_negative_step & final_not_best ~ "negative_step + final_not_best",
        final_prefix_low ~ "final_prefix_low",
        has_negative_step ~ "negative_step",
        final_not_best ~ "final_not_best",
        TRUE ~ ""
      )
    )

  list(
    pair_level = pair_level,
    path_level = path_level
  )
}

# ------------------------------------------------------------
# 6) Convenience wrapper
# ------------------------------------------------------------

#' Run combined embedding-based tag-path validation
#'
#' @param tag_matrix_clean Cleaned tag matrix with tag_level_* columns.
#' @param model Embedding model name.
#' @param base_url Ollama base URL.
#' @param timeout_sec Timeout in seconds.
#' @param min_tag_sim Minimum acceptable tag similarity.
#' @param min_prefix_sim Minimum acceptable final prefix similarity.
#' @param max_negative_delta Maximum tolerated negative prefix delta.
#' @param progress Print progress.
#'
#' @return Named list of all intermediate and final results.
run_tag_path_validation <- function(tag_matrix_clean,
                                    model,
                                    base_url,
                                    timeout_sec = 60,
                                    min_tag_sim = 0.30,
                                    min_prefix_sim = 0.40,
                                    max_negative_delta = -0.05,
                                    progress = TRUE) {
  audit_df <- build_audit_paths_from_clean(tag_matrix_clean)

  question_tag_long <- build_question_tag_long(audit_df, path_col = "after_path")
  question_prefix_long <- build_question_prefix_long(audit_df, path_col = "after_path")

  question_emb <- embed_questions(
    audit_df = audit_df,
    model = model,
    base_url = base_url,
    timeout_sec = timeout_sec,
    progress = progress
  )

  tag_emb <- embed_tags_for_validation(
    question_tag_long = question_tag_long,
    model = model,
    base_url = base_url,
    timeout_sec = timeout_sec,
    progress = progress
  )

  prefix_emb <- embed_prefixes_for_validation(
    question_prefix_long = question_prefix_long,
    model = model,
    base_url = base_url,
    timeout_sec = timeout_sec,
    progress = progress
  )

  tag_scores <- score_question_tag_similarity(
    question_tag_long = question_tag_long,
    question_emb = question_emb,
    tag_emb = tag_emb
  )

  prefix_scores <- score_question_prefix_similarity(
    question_prefix_long = question_prefix_long,
    question_emb = question_emb,
    prefix_emb = prefix_emb
  )

  validation <- validate_question_tag_paths(
    tag_scores = tag_scores,
    prefix_scores = prefix_scores,
    min_tag_sim = min_tag_sim,
    min_prefix_sim = min_prefix_sim,
    max_negative_delta = max_negative_delta
  )

  list(
    audit_df = audit_df,
    question_tag_long = question_tag_long,
    question_prefix_long = question_prefix_long,
    question_emb = question_emb,
    tag_emb = tag_emb,
    prefix_emb = prefix_emb,
    tag_scores = tag_scores,
    prefix_scores = prefix_scores,
    pair_level = validation$pair_level,
    path_level = validation$path_level
  )
}
