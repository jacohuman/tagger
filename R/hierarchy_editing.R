#' Manual hierarchy editing helpers for BERTopic/cluster tag trees
#'
#' These utilities are intended to run after the main tagging pipeline has
#' produced a `tag_state`. They support surgical edits (pruning unwanted tags)
#' and targeted re-classification (refining an overly broad leaf cluster).

#' Validate that a state contains the fields needed for hierarchy editing
#' @keywords internal
validate_editable_state <- function(state) {
  if (!inherits(state, "tag_state")) {
    stop("state must inherit from 'tag_state'.", call. = FALSE)
  }
  if (is.null(state$assignments) || is.null(state$clusters)) {
    stop("state must include non-NULL assignments and clusters.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Rebuild cluster index while ignoring missing cluster IDs
#' @keywords internal
rebuild_cluster_index <- function(assignments) {
  lvl_cols <- cluster_level_cols(assignments)
  if (length(lvl_cols) == 0) {
    stop("No cluster_level_* columns found in assignments.", call. = FALSE)
  }

  max_level <- length(lvl_cols)
  out <- purrr::map_dfr(seq_along(lvl_cols), function(level_idx) {
    this_col <- lvl_cols[[level_idx]]
    parent_col <- if (level_idx < max_level) lvl_cols[[level_idx + 1]] else NA_character_

    assignments |>
      dplyr::filter(!is.na(.data[[this_col]])) |>
      dplyr::group_by(cluster_id = .data[[this_col]]) |>
      dplyr::summarise(
        level = level_idx,
        parent_cluster = if (!is.na(parent_col)) {
          p <- unique(.data[[parent_col]])
          p <- p[!is.na(p)]
          if (length(p) == 0) NA_integer_ else as.integer(p[[1]])
        } else {
          NA_integer_
        },
        question_ids = list(id),
        .groups = "drop"
      )
  }) |>
    dplyr::arrange(level, cluster_id) |>
    dplyr::mutate(tag = NA_character_)

  out
}

#' Restore cluster tags after a structural edit
#' @keywords internal
restore_cluster_tags <- function(clusters, previous_clusters) {
  tag_lookup <- previous_clusters |>
    dplyr::select(level, cluster_id, tag)

  clusters |>
    dplyr::left_join(tag_lookup, by = c("level", "cluster_id"), suffix = c("", "_old")) |>
    dplyr::mutate(tag = dplyr::coalesce(.data$tag_old, .data$tag)) |>
    dplyr::select(-.data$tag_old)
}

#' Refresh derived artifacts after hierarchy edits
#'
#' @param state A `tag_state`.
#' @return Updated state with refreshed `clusters` and `tag_matrix`.
#' @export
refresh_tag_state_artifacts <- function(state) {
  validate_editable_state(state)

  previous_clusters <- state$clusters
  clusters <- rebuild_cluster_index(state$assignments)
  clusters <- restore_cluster_tags(clusters, previous_clusters)

  state$clusters <- clusters
  state$tag_matrix <- build_question_tag_matrix(state$assignments, state$clusters)
  state$cleaned <- NULL
  state$audit <- NULL

  state
}

#' Prune an unwanted tag from the hierarchy
#'
#' @param state A `tag_state` returned by a tagging pipeline.
#' @param tag Character scalar tag name to remove.
#' @param level Optional integer level to disambiguate repeated tags.
#' @param collapse_empty_levels Logical; if TRUE, drops any level columns that
#'   become all-missing after pruning.
#'
#' @return Updated `tag_state` with the tag removed and derived artifacts
#'   refreshed.
#' @export
prune_tag_from_state <- function(state, tag, level = NULL, collapse_empty_levels = TRUE) {
  validate_editable_state(state)

  hits <- state$clusters |>
    dplyr::filter(.data$tag == !!tag)

  if (!is.null(level)) {
    hits <- hits |>
      dplyr::filter(.data$level == !!as.integer(level))
  }

  if (nrow(hits) == 0) {
    stop("No matching cluster tag found to prune.", call. = FALSE)
  }

  assignments <- state$assignments
  lvl_cols <- cluster_level_cols(assignments)

  for (i in seq_len(nrow(hits))) {
    lvl <- as.integer(hits$level[[i]])
    cid <- as.integer(hits$cluster_id[[i]])
    col <- paste0("cluster_level_", lvl)
    if (col %in% lvl_cols) {
      assignments[[col]][assignments[[col]] == cid] <- NA_integer_
    }
  }

  if (isTRUE(collapse_empty_levels)) {
    empty_cols <- purrr::keep(lvl_cols, ~ all(is.na(assignments[[.x]])))
    if (length(empty_cols) > 0) {
      assignments <- assignments |>
        dplyr::select(-dplyr::all_of(empty_cols))

      new_lvl_cols <- cluster_level_cols(assignments)
      if (length(new_lvl_cols) > 0) {
        for (i in seq_along(new_lvl_cols)) {
          names(assignments)[names(assignments) == new_lvl_cols[[i]]] <- paste0("cluster_level_", i)
        }
      }
    }
  }

  state$assignments <- assignments
  refresh_tag_state_artifacts(state)
}

#' Refine one broad leaf cluster by re-running BERTopic + tagging on its subset
#'
#' @param state A `tag_state`.
#' @param cluster_id Leaf-cluster identifier at `level = 1`.
#' @param sample_size Number of representative captions per cluster for tagging.
#' @param config Ollama config from [ollama_config()].
#' @param topic_levels Optional hierarchy vector for the re-run subset.
#' @param bertopic_kwargs Optional BERTopic constructor kwargs.
#'
#' @details
#' This helper currently supports refining level-1 (leaf) clusters. It inserts
#' one new leaf level beneath the existing level-1 assignments. Questions not in
#' the refined cluster keep their previous leaf tag as their new leaf tag.
#'
#' @return Updated `tag_state` with one additional leaf level and updated tags.
#' @export
refine_leaf_cluster_with_bertopic <- function(
    state,
    cluster_id,
    sample_size = 5,
    config = ollama_config(),
    topic_levels = NULL,
    bertopic_kwargs = list()
) {
  validate_editable_state(state)

  assignments <- state$assignments
  if (!"cluster_level_1" %in% names(assignments)) {
    stop("state assignments must contain cluster_level_1.", call. = FALSE)
  }

  cluster_id <- as.integer(cluster_id)
  target_rows <- which(assignments$cluster_level_1 == cluster_id)
  if (length(target_rows) < 2) {
    stop("Target cluster must contain at least 2 questions to refine.", call. = FALSE)
  }

  subset_questions <- assignments[target_rows, c("id", "caption"), drop = FALSE]
  subset_embeddings <- NULL
  if (!is.null(state$embeddings)) {
    subset_embeddings <- state$embeddings[target_rows, , drop = FALSE]
  }

  bertopic <- fit_bertopic_topics(
    texts = subset_questions$caption,
    embeddings = subset_embeddings,
    bertopic_kwargs = bertopic_kwargs
  )

  subset_questions$topic_id <- bertopic$topic_ids
  hierarchy <- build_bertopic_hierarchy(
    questions = subset_questions,
    question_embeddings = subset_embeddings,
    topic_levels = topic_levels
  )

  sub_state <- structure(
    list(
      questions = hierarchy$questions,
      assignments = hierarchy$assignments,
      clusters = hierarchy$clusters
    ),
    class = "tag_state"
  )

  sub_state <- tag_clusters_bottom_up(
    state = sub_state,
    sample_size = sample_size,
    progress_path = tempfile(fileext = ".rds"),
    model = config$tagger_model,
    base_url = config$base_url
  )

  old_lvl_cols <- cluster_level_cols(assignments)

  assignments <- assignments |>
    dplyr::mutate(cluster_level_0 = .data$cluster_level_1)

  # Re-number columns so inserted level becomes cluster_level_1.
  for (i in rev(seq_along(old_lvl_cols))) {
    old_name <- paste0("cluster_level_", i)
    new_name <- paste0("cluster_level_", i + 1L)
    names(assignments)[names(assignments) == old_name] <- new_name
  }
  names(assignments)[names(assignments) == "cluster_level_0"] <- "cluster_level_1"

  # Build mapping for refined subset new leaf ids and keep everyone else stable.
  max_old_leaf <- max(as.integer(assignments$cluster_level_2), na.rm = TRUE)
  sub_leaf <- sub_state$assignments[, c("id", "cluster_level_1"), drop = FALSE]
  sub_leaf <- sub_leaf |>
    dplyr::mutate(cluster_level_1 = as.integer(.data$cluster_level_1) + max_old_leaf)

  row_map <- match(assignments$id, sub_leaf$id)
  hit <- which(!is.na(row_map))
  assignments$cluster_level_1[hit] <- sub_leaf$cluster_level_1[row_map[hit]]

  state$assignments <- assignments
  state <- refresh_tag_state_artifacts(state)

  # Carry over newly generated refined leaf tags.
  refined_tags <- sub_state$clusters |>
    dplyr::filter(.data$level == 1L) |>
    dplyr::transmute(
      level = 1L,
      cluster_id = as.integer(.data$cluster_id) + max_old_leaf,
      tag = .data$tag
    )

  state$clusters <- state$clusters |>
    dplyr::left_join(refined_tags, by = c("level", "cluster_id"), suffix = c("", "_refined")) |>
    dplyr::mutate(tag = dplyr::coalesce(.data$tag_refined, .data$tag)) |>
    dplyr::select(-.data$tag_refined)

  state$tag_matrix <- build_question_tag_matrix(state$assignments, state$clusters)
  state$cleaned <- NULL
  state$audit <- NULL

  state
}

#' Validate hierarchy consistency after manual edits
#'
#' @param state A `tag_state`.
#' @return A list with quick diagnostics.
#' @export
validate_hierarchy_integrity <- function(state) {
  validate_editable_state(state)

  lvl_cols <- cluster_level_cols(state$assignments)
  missing_tags <- state$clusters |>
    dplyr::filter(is.na(.data$tag) | !nzchar(.data$tag))

  orphan_counts <- purrr::map_dfr(seq_along(lvl_cols)[-1], function(lvl) {
    child_col <- paste0("cluster_level_", lvl - 1L)
    parent_col <- paste0("cluster_level_", lvl)

    state$assignments |>
      dplyr::filter(!is.na(.data[[child_col]]) & is.na(.data[[parent_col]])) |>
      dplyr::summarise(level = lvl, orphan_questions = dplyr::n())
  })

  list(
    n_questions = nrow(state$assignments),
    n_levels = length(lvl_cols),
    untagged_clusters = nrow(missing_tags),
    orphan_counts = orphan_counts
  )
}
