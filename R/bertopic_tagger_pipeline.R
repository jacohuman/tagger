#' BERTopic-driven tagging pipeline with shared hierarchical tagging logic
#'
#' @description
#' Builds question clusters with BERTopic, then reuses the existing tagging,
#' cleanup, synonym-review, and hierarchy-audit helpers from this package.
#' This file is additive and does not alter the existing pipeline.
#'
#' @param forms Tibble with nested `form` list-column.
#' @param limit_n Maximum number of unique questions to include.
#' @param sample_size Number of sample questions sent per cluster tagging call.
#' @param progress_path Optional path to persist intermediate state.
#' @param topic_levels Optional integer vector for hierarchy levels.
#'   If NULL, levels are inferred from the number of BERTopic topics.
#' @param reviewed_groups Optional reviewed synonym-group table. If NULL,
#'   synonym collapse is not applied (human review is required first).
#' @param min_synonym_sim Minimum cosine threshold for synonym candidates.
#' @param auto_synonym_sim "very_high" confidence threshold.
#' @param config Output of [ollama_config()].
#' @param bertopic_kwargs Optional named list passed to `BERTopic(...)`.
#'
#' @return Named list with BERTopic outputs, tagging state, cleanup artifacts,
#'   synonym-review candidates, and reordered tag matrix.
#' @export
run_bertopic_tagger_pipeline <- function(
    forms,
    limit_n = Inf,
    sample_size = 6,
    progress_path = NULL,
    topic_levels = NULL,
    reviewed_groups = NULL,
    min_synonym_sim = 0.88,
    auto_synonym_sim = 0.94,
    config = ollama_config(),
    bertopic_kwargs = list()
) {
  questions <- unnest_questions(forms, limit_n = limit_n)

  if (!is.null(progress_path) && file.exists(progress_path)) {
    state <- load_tag_state(progress_path)
    if (!inherits(state, "tag_state")) {
      stop("Progress file is not a tag_state object. Remove it to restart BERTopic tagging.", call. = FALSE)
    }
  } else {
    question_embeddings <- embed_multiple_questions(
      texts = questions$caption,
      model = config$embed_model,
      base_url = config$base_url
    )
    embedding_matrix <- do.call(rbind, question_embeddings)

    bertopic <- fit_bertopic_topics(
      texts = questions$caption,
      embeddings = embedding_matrix,
      bertopic_kwargs = bertopic_kwargs
    )

    questions$topic_id <- bertopic$topic_ids

    hierarchy <- build_bertopic_hierarchy(
      questions = questions,
      question_embeddings = embedding_matrix,
      topic_levels = topic_levels
    )

    state <- structure(list(
      questions = hierarchy$questions,
      assignments = hierarchy$assignments,
      clusters = hierarchy$clusters,
      clusters_by_level = hierarchy$clusters_by_level
    ), class = "tag_state")

    if (!is.null(progress_path)) {
      save_tag_state(state, progress_path)
    }
  }

  state <- tag_clusters_bottom_up(
    state = state,
    sample_size = sample_size,
    progress_path = if (is.null(progress_path)) tempfile(fileext = '.rds') else progress_path,
    model = config$tagger_model,
    base_url = config$base_url
  )

  tag_matrix <- build_question_tag_matrix(state$assignments, state$clusters)
  clean_res <- clean_tag_matrix(tag_matrix)

  tag_registry <- build_tag_registry_from_clean(clean_res$tag_matrix_clean)
  tag_emb <- embed_tag_registry(
    tag_registry = tag_registry,
    model = config$embed_model,
    base_url = config$base_url
  )

  synonym_pairs <- find_synonym_candidates_embeddings(
    tag_registry = tag_registry,
    tag_emb = tag_emb,
    min_sim = min_synonym_sim,
    auto_sim = auto_synonym_sim
  )

  synonym_groups <- build_synonym_group_candidates(
    cands = synonym_pairs,
    tag_registry = tag_registry
  )
  synonym_groups <- summarise_synonym_groups(
    group_candidates = synonym_groups,
    cands = synonym_pairs
  )

  collapsed <- clean_res$tag_matrix_clean
  synonym_status <- "pending_human_review"

  if (!is.null(reviewed_groups)) {
    tag_map <- build_tag_map_from_group_review(reviewed_groups)
    collapsed_res <- apply_synonym_collapse(
      tag_matrix_clean = clean_res$tag_matrix_clean,
      tag_map = tag_map
    )
    collapsed <- collapsed_res$tag_matrix_clean
    synonym_status <- "applied_from_review"
  }

  reordered <- reorder_tag_matrix_by_support(collapsed)

  out <- list(
    bertopic = if (exists("bertopic")) bertopic else NULL,
    state = state,
    tag_matrix = tag_matrix,
    clean = clean_res,
    tag_registry = tag_registry,
    tag_embeddings = tag_emb,
    synonym_pairs = synonym_pairs,
    synonym_groups_for_review = synonym_groups,
    synonym_status = synonym_status,
    tag_matrix_post_synonyms = collapsed,
    reorder = reordered,
    tag_matrix_final = reordered$tag_matrix_clean
  )

  out
}

#' Fit BERTopic and return per-document topic IDs
#'
#' @param texts Character vector of question captions.
#' @param embeddings Optional numeric matrix of precomputed embeddings.
#'   When supplied, BERTopic skips its own embedding model.
#' @param bertopic_kwargs Named list forwarded to BERTopic constructor.
#'
#' @return List with topic_ids and Python objects used for tracing.
#' @export
fit_bertopic_topics <- function(texts, embeddings = NULL, bertopic_kwargs = list()) {
  ensure_bertopic_python()

  texts <- as.character(texts)
  texts <- texts[!is.na(texts)]
  texts <- stringr::str_squish(texts)
  texts <- texts[nzchar(texts)]

  bertopic_mod <- reticulate::import("bertopic")

  default_kwargs <- list(
    calculate_probabilities = FALSE,
    verbose = TRUE
  )
  kwargs <- utils::modifyList(default_kwargs, bertopic_kwargs)

  model <- do.call(bertopic_mod$BERTopic, kwargs)

  fit <- if (is.null(embeddings)) {
    model$fit_transform(texts)
  } else {
    model$fit_transform(texts, embeddings = embeddings)
  }

  topic_ids <- tryCatch(
    reticulate::py_to_r(fit[[1]]),
    error = function(e) reticulate::py_to_r(fit)
  )

  topic_ids <- as.integer(topic_ids)

  list(
    topic_ids = topic_ids,
    model = model,
    fit_raw = fit
  )
}

#' Build hierarchical assignments from BERTopic leaf topics
#'
#' @param questions Tibble with `id`, `caption`, and `topic_id`.
#' @param question_embeddings List/matrix of question embeddings.
#' @param topic_levels Optional full hierarchy vector.
#'
#' @return List with questions, assignments, clusters, and clusters_by_level.
#' @export
build_bertopic_hierarchy <- function(questions, question_embeddings, topic_levels = NULL) {
  stopifnot(all(c("id", "caption", "topic_id") %in% names(questions)))

  topic_map <- sort(unique(questions$topic_id))
  topic_idx <- seq_along(topic_map)
  names(topic_idx) <- as.character(topic_map)

  # Ensure leaf cluster ids are contiguous positive integers.
  leaf_cluster <- unname(topic_idx[as.character(questions$topic_id)])

  emb_matrix <- if (is.list(question_embeddings)) {
    do.call(rbind, question_embeddings)
  } else {
    question_embeddings
  }

  questions$embedding <- lapply(seq_len(nrow(emb_matrix)), function(i) emb_matrix[i, ])

  assignments <- questions[, c("id", "caption"), drop = FALSE]
  assignments$cluster_level_1 <- as.integer(leaf_cluster)

  n_leaf <- length(topic_map)

  if (is.null(topic_levels)) {
    topic_levels <- infer_topic_levels(n_leaf)
  }

  # Keep first level fixed to BERTopic leaf clusters.
  topic_levels[[1]] <- n_leaf

  if (length(topic_levels) > 1 && n_leaf > 1) {
    topic_centroids <- lapply(seq_len(n_leaf), function(k) {
      rows <- which(leaf_cluster == k)
      colMeans(emb_matrix[rows, , drop = FALSE])
    })

    topic_tree <- stats::hclust(stats::dist(do.call(rbind, topic_centroids)), method = "ward.D2")

    for (lvl in seq_along(topic_levels)[-1]) {
      k <- topic_levels[[lvl]]
      topic_group <- stats::cutree(topic_tree, k = k)
      assignments[[paste0("cluster_level_", lvl)]] <- topic_group[leaf_cluster]
    }
  }

  clusters <- build_cluster_index(assignments, clusters_by_level = topic_levels)

  list(
    questions = questions,
    assignments = assignments,
    clusters = clusters,
    clusters_by_level = topic_levels
  )
}

#' Infer a descending hierarchy from number of leaf topics
#'
#' @param n_leaf Number of leaf clusters/topics.
#'
#' @return Integer vector like c(n_leaf, ..., 2).
#' @export
infer_topic_levels <- function(n_leaf) {
  if (n_leaf <= 1) return(1L)

  ks <- c(n_leaf)
  cur <- n_leaf
  while (cur > 2) {
    cur <- max(2L, as.integer(ceiling(cur / 2)))
    ks <- c(ks, cur)
    if (cur == 2L) break
  }
  unique(as.integer(ks))
}
