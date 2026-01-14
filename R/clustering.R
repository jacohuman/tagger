#' Build binary clustering levels up to k
#'
#' @param k_deep Desired number of clusters at the deepest level (this must be a power of 2).
#'
#' @return Integer vector like c(k, k/2, ..., 2) for use as clusters_by_level.
#'         Level 1 = most granular (k clusters), last level = 2 clusters.
#' @export
binary_levels <- function(k_deep) {
  if (k_deep < 2) stop("Number of clusters at the deepest level (k_deep) must be >= 2.")

  # Enforce power-of-two for clean binary splits
  if (bitwAnd(k_deep, k_deep - 1L) != 0L) {
    stop("Number of clusters at the deepest level (k_deep) must be a power of 2 (2, 4, 8, 16, ...).")
  }

  # 2, 4, 8, ..., k_deep  ->  k_deep, ..., 4, 2
  levels <- 2L^(1L:as.integer(log2(k_deep)))
  rev(levels)
}


#' Build hierarchical clustering assignments for questions
#'
#' @description
#' Internal helper that embeds question texts and returns per-level cluster
#' assignments. The embeddings are retained in the question tibble for downstream
#' reuse.
#'
#' @inheritParams run_question_tagger
#'
#' @return A list with \code{questions} and \code{assignments} tibbles.
#'
build_question_hierarchy <- function(forms, clusters_by_level, limit_n, embed_model, base_url) {
  questions <- unnest_questions(forms, limit_n)
  embeddings <- embed_multiple_questions(
    texts = questions$caption,
    model = embed_model,
    base_url = base_url
  )
  questions$embedding <- embeddings

  clustering <- cluster_embeddings(embeddings, clusters_by_level)
  assignments <- add_cluster_assignments(questions, clustering$hclust, clusters_by_level)

  list(questions = questions, assignments = assignments, hclust = clustering$hclust)
}

#' Cluster embeddings and return hclust object
#'
#' @param embeddings List or matrix of embeddings.
#' @param clusters_by_level Integer vector of cluster counts by level.
#'
#' @return A list with hclust and distance matrix.
#' @export
cluster_embeddings <- function(embeddings, clusters_by_level) {
  if (is.list(embeddings)) {
    emb_matrix <- do.call(rbind, embeddings)
  } else {
    emb_matrix <- embeddings
  }
  tree <- stats::hclust(stats::dist(emb_matrix), method = "ward.D2")
  list(hclust = tree, dist = stats::dist(emb_matrix), clusters_by_level = clusters_by_level)
}

#' Add cluster assignments to question table
#'
#' @param questions Tibble with question data.
#' @param hclust Hierarchical clustering object.
#' @param clusters_by_level Integer vector of cluster counts by level.
#'
#' @return Tibble with cluster_level_* columns.
#' @export
add_cluster_assignments <- function(questions, hclust, clusters_by_level) {
  level_cols <- paste0("cluster_level_", seq_along(clusters_by_level))
  assignments <- questions
  for (i in seq_along(clusters_by_level)) {
    assignments[[level_cols[[i]]]] <- stats::cutree(hclust, k = clusters_by_level[[i]])
  }
  assignments
}


#' Summarise cluster memberships across levels
#'
#' @description
#' Creates a per-cluster index that includes parent links and question ids.
#'
#' @inheritParams run_question_tagger
#' @param assignments Tibble returned from [build_question_hierarchy()].
#'
#' @return A tibble with one row per cluster and columns:
#'   \itemize{
#'     \item \code{level}
#'     \item \code{cluster_id}
#'     \item \code{parent_cluster}
#'     \item \code{question_ids}
#'     \item \code{tag}
#'   }
#' @keywords internal
build_cluster_index <- function(assignments, clusters_by_level) {
  level_cols <- paste0("cluster_level_", seq_along(clusters_by_level))
  max_level <- length(level_cols)

  purrr::map_dfr(seq_along(level_cols), function(level_idx) {
    this_col <- level_cols[[level_idx]]
    parent_col <- if (level_idx < max_level) level_cols[[level_idx + 1]] else NA_character_

    assignments %>%
      group_by(cluster_id = .data[[this_col]]) %>%
      summarise(
        level = level_idx,
        parent_cluster = if (!is.na(parent_col)) unique(.data[[parent_col]])[1] else NA_integer_,
        question_ids = list(id),
        .groups = "drop"
      ) %>%
      mutate(tag = NA_character_)
  }) %>%
    arrange(level, cluster_id)
}
