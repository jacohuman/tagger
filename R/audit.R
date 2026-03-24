library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(stringi)
library(tibble)

# ============================================================
# Tag auditing pipeline (deterministic only)
#
# Current implemented checks:
#  1) Deterministic canonicalization via canon_tag()
#  2) Generality check using monotonicity
#
# Expected input:
#   audit_final <- state$tag_matrix_clean %>%
#     dplyr::select(id, caption) %>%
#     dplyr::mutate(
#       after_path = purrr::pmap_chr(
#         dplyr::select(state$tag_matrix_clean, dplyr::matches("^tag_level_")),
#         ~ paste(stats::na.omit(c(...)), collapse = " > ")
#       )
#     )
# ============================================================

# -------------------------------
# 1) Path utilities
# -------------------------------

#' Split a tag path string into a character vector
#'
#' @description
#' Converts a path like "a > b > c" into c("a","b","c").
#' Handles missing/empty values by returning an empty character vector.
#'
#' @param path A single character string representing the tag path.
#' @param sep The separator between tags (default " > ").
#'
#' @return A character vector of tags (possibly length 0).
#' @keywords internal
split_path <- function(path, sep = " > ") {
  if (length(path) == 0 || is.na(path) || !nzchar(path)) {
    return(character(0))
  }

  parts <- str_split(path, fixed(sep), simplify = FALSE)[[1]]
  parts <- str_trim(parts)
  parts <- parts[parts != ""]
  parts
}

#' Join a character vector of tags into a path string
#'
#' @description
#' Converts c("a","b","c") into "a > b > c" while removing
#' NA and empty entries.
#'
#' @param tags A character vector of tags.
#' @param sep The separator to use (default " > ").
#'
#' @return A single character string representing the path.
#' @keywords internal
join_path <- function(tags, sep = " > ") {
  tags <- as.character(tags)
  tags <- tags[!is.na(tags)]
  tags <- str_trim(tags)
  tags <- tags[tags != ""]
  paste(tags, collapse = sep)
}

# -------------------------------
# 2) Long form + registry
# -------------------------------

#' Build a long-form tag table from an audit tibble
#'
#' @description
#' Takes a tibble with id, caption, and a path column
#' (default after_path), and expands it into one row per tag per question.
#'
#' This function applies canon_tag() to each tag.
#'
#' @param audit_df A tibble containing at least id, caption, and a path column.
#' @param path_col Name of the column containing the cleaned path string.
#'
#' @return A tibble with columns id, caption, after_path, tag, pos.
#' @export
build_long_from_audit <- function(audit_df, path_col = "after_path") {
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
          tag = character(0),
          pos = integer(0)
        ))
      }

      tibble::tibble(
        id = id,
        caption = caption,
        after_path = after_path,
        tag = tags,
        pos = seq_along(tags)
      )
    }
  )
}

#' Build a tag registry from long-form tags
#'
#' @description
#' Produces a frequency table of tags using question-level support.
#'
#' @param tag_long A long-form tibble produced by build_long_from_audit().
#'
#' @return A tibble with columns tag and n.
#' @export
build_tag_registry <- function(tag_long) {
  stopifnot(all(c("id", "tag") %in% names(tag_long)))

  tag_long %>%
    dplyr::distinct(id, tag) %>%
    dplyr::count(tag, name = "n", sort = TRUE)
}

# -------------------------------
# 3) Generality check via monotonicity
# -------------------------------

#' Audit hierarchy order violations using support monotonicity
#'
#' @description
#' Uses a simple data-driven heuristic:
#' more generic tags should tend to have wider question-level support.
#'
#' For a path:
#'   generic > more specific > even more specific
#'
#' support should usually be non-increasing along the path.
#'
#' So a likely violation occurs when support increases between adjacent steps.
#'
#' @param tag_long A long-form tibble produced by build_long_from_audit().
#'
#' @return A tibble of paths with hierarchy violations.
#' @export
audit_hierarchy <- function(tag_long) {
  stopifnot(all(c("id", "caption", "after_path", "tag", "pos") %in% names(tag_long)))

  if (nrow(tag_long) == 0) {
    return(tibble::tibble(
      id = character(),
      caption = character(),
      after_path = character(),
      tags = list(),
      pos = list(),
      support = list(),
      violations = list(),
      n_violations = integer(),
      violation_pair = character()
    ))
  }

  # Question-level global support per tag
  support_tbl <- tag_long %>%
    dplyr::distinct(id, tag) %>%
    dplyr::count(tag, name = "support_global")

  by_path <- tag_long %>%
    dplyr::left_join(support_tbl, by = "tag") %>%
    dplyr::arrange(id, pos) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      caption = dplyr::first(caption),
      after_path = dplyr::first(after_path),
      tags = list(tag),
      pos = list(pos),
      support = list(support_global),
      .groups = "drop"
    )

  by_path %>%
    dplyr::mutate(
      violations = purrr::map(support, function(s) {
        if (length(s) <= 1) return(integer(0))
        which(diff(s) > 0) + 1L
      }),
      n_violations = purrr::map_int(violations, length),
      violation_pair = purrr::pmap_chr(
        list(tags, support, violations),
        function(tags_i, support_i, viol_i) {
          if (length(viol_i) == 0) return("")

          parts <- purrr::map_chr(viol_i, function(i) {
            if (i <= 1 || i > length(tags_i)) return(NA_character_)

            prev_tag <- tags_i[[i - 1]]
            next_tag <- tags_i[[i]]
            prev_sup <- support_i[[i - 1]]
            next_sup <- support_i[[i]]

            paste0(prev_tag, " (", prev_sup, ") -> ", next_tag, " (", next_sup, ")")
          })

          parts <- parts[!is.na(parts)]
          paste(parts, collapse = "; ")
        }
      )
    ) %>%
    dplyr::filter(n_violations > 0) %>%
    dplyr::arrange(dplyr::desc(n_violations))
}

# -------------------------------
# 4) Combined deterministic audit report
# -------------------------------

#' Build a deterministic tag audit report
#'
#' @description
#' Runs the currently enabled audit checks over cleaned paths:
#' 1. deterministic canonicalization via canon_tag()
#' 2. generality check using support monotonicity
#'
#' @param audit_df A tibble containing at least id, caption, and after_path.
#' @param path_col Name of the path column.
#'
#' @return A named list containing:
#'   - tag_long
#'   - tag_registry
#'   - hierarchy_violations
#' @export
audit_report_deterministic <- function(audit_df, path_col = "after_path") {
  tag_long <- build_long_from_audit(audit_df, path_col = path_col)
  registry <- build_tag_registry(tag_long)
  h <- audit_hierarchy(tag_long)

  list(
    tag_long = tag_long,
    tag_registry = registry,
    hierarchy_violations = h
  )
}

# -------------------------------
# Reorder paths by global tag support
# -------------------------------

#' Reorder a single tag vector by descending global support
#'
#' @param tags Character vector of tags for one row/path.
#' @param support_map Named numeric vector: names are tags, values are support.
#'
#' @return Character vector reordered from highest support to lowest support.
#' @keywords internal
reorder_tags_by_support <- function(tags, support_map) {
  tags <- canon_tag(tags)
  tags <- tags[!is.na(tags)]
  tags <- tags[tags != ""]

  if (length(tags) <= 1) return(tags)

  supp <- unname(support_map[tags])
  supp[is.na(supp)] <- 0

  # stable sort: highest support first, preserve original order on ties
  ord <- order(-supp, seq_along(tags))
  tags[ord]
}

#' Reorder one path string by descending global tag support
#'
#' @param path A single path string like "a > b > c".
#' @param support_map Named numeric vector: names are tags, values are support.
#' @param sep Path separator.
#'
#' @return Reordered path string.
#' @export
reorder_path_by_support <- function(path, support_map, sep = " > ") {
  tags <- split_path(path, sep = sep)
  tags <- reorder_tags_by_support(tags, support_map = support_map)
  join_path(tags, sep = sep)
}

#' Reorder all paths in an audit tibble by descending global tag support
#'
#' @description
#' Computes question-level global support for each tag, then reorders each row's
#' path so tags go from most-used to least-used.
#'
#' @param audit_df Tibble containing at least id, caption, and a path column.
#' @param path_col Name of the path column.
#'
#' @return A list with:
#'   - audit_reordered: original tibble plus reordered path columns
#'   - tag_registry: tag registry used for support
#'   - support_map: named numeric vector of support values
#' @export
reorder_audit_paths_by_support <- function(audit_df, path_col = "after_path") {
  tag_long <- build_long_from_audit(audit_df, path_col = path_col)

  registry <- build_tag_registry(tag_long)

  support_map <- registry$n
  names(support_map) <- registry$tag

  out <- audit_df %>%
    dplyr::mutate(
      original_path = .data[[path_col]],
      reordered_path = purrr::map_chr(
        .data[[path_col]],
        ~ reorder_path_by_support(.x, support_map = support_map)
      ),
      path_changed = original_path != reordered_path
    )

  list(
    audit_reordered = out,
    tag_registry = registry,
    support_map = support_map
  )
}

#' Apply support-based reordering back to tag_level_* columns
#'
#' @param tag_matrix_clean Cleaned tag matrix with tag_level_* columns.
#' @param tag_cols Optional tag columns in order.
#'
#' @return List with updated tag_matrix_clean, audit_reordered, and tag_registry.
#' @export
reorder_tag_matrix_by_support <- function(tag_matrix_clean, tag_cols = NULL) {
  if (is.null(tag_cols)) {
    tag_cols <- grep("^tag_level_", names(tag_matrix_clean), value = TRUE)
  }
  stopifnot(length(tag_cols) > 0)

  audit_df <- tag_matrix_clean %>%
    dplyr::select(id, caption) %>%
    dplyr::mutate(
      after_path = purrr::pmap_chr(
        dplyr::select(tag_matrix_clean, dplyr::all_of(tag_cols)),
        ~ paste(stats::na.omit(c(...)), collapse = " > ")
      )
    )

  res <- reorder_audit_paths_by_support(audit_df, path_col = "after_path")

  updated_paths <- res$audit_reordered %>%
    dplyr::transmute(
      id,
      reordered_tags = purrr::map(reordered_path, split_path)
    ) %>%
    dplyr::mutate(
      reordered_tags = purrr::map(reordered_tags, function(tags) {
        k <- length(tag_cols)
        out <- c(tags, rep(NA_character_, max(0, k - length(tags))))[1:k]
        names(out) <- tag_cols
        out
      })
    ) %>%
    tidyr::unnest_wider(reordered_tags)

  updated <- tag_matrix_clean %>%
    dplyr::select(-dplyr::all_of(tag_cols)) %>%
    dplyr::left_join(updated_paths, by = "id") %>%
    dplyr::relocate(dplyr::all_of(tag_cols), .after = caption)

  list(
    tag_matrix_clean = updated,
    audit_reordered = res$audit_reordered,
    tag_registry = res$tag_registry
  )
}


