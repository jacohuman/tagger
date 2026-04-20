test_that("infer_topic_levels returns a descending hierarchy", {
  expect_equal(infer_topic_levels(1), 1L)
  expect_equal(infer_topic_levels(2), 2L)
  expect_equal(infer_topic_levels(5), c(5L, 3L, 2L))
  expect_equal(infer_topic_levels(8), c(8L, 4L, 2L))
})

test_that("build_bertopic_hierarchy builds assignment and cluster index", {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("A", "B", "C", "D"),
    topic_id = c(20L, 20L, 7L, 7L)
  )

  embeddings <- matrix(
    c(
      1, 0,
      1, 0.1,
      0, 1,
      0.1, 1
    ),
    byrow = TRUE,
    ncol = 2
  )

  out <- build_bertopic_hierarchy(
    questions = questions,
    question_embeddings = embeddings,
    topic_levels = c(2L, 2L)
  )

  expect_equal(out$clusters_by_level, c(2L, 2L))
  expect_true(all(c("cluster_level_1", "cluster_level_2") %in% names(out$assignments)))
  expect_setequal(unique(out$assignments$cluster_level_1), c(1L, 2L))
  expect_s3_class(out$clusters, "data.frame")
  expect_true(all(c("level", "cluster_id", "parent_cluster", "question_ids", "tag") %in% names(out$clusters)))
})

test_that("run_bertopic_tagger_pipeline orchestrates pipeline from fresh start", {
  fake_questions <- tibble::tibble(id = c("q1", "q2"), caption = c("What is age?", "What is income?"))
  fake_state <- structure(
    list(
      questions = fake_questions,
      assignments = tibble::tibble(id = c("q1", "q2"), caption = c("What is age?", "What is income?"), cluster_level_1 = c(1L, 2L)),
      clusters = tibble::tibble(level = 1L, cluster_id = c(1L, 2L), parent_cluster = NA_integer_, question_ids = list("q1", "q2"), tag = c("demo_age", "demo_income")),
      clusters_by_level = 2L
    ),
    class = "tag_state"
  )

  testthat::local_mocked_bindings(
    unnest_questions = function(forms, limit_n = Inf) fake_questions,
    embed_multiple_questions = function(texts, model, base_url) list(c(1, 0), c(0, 1)),
    fit_bertopic_topics = function(texts, embeddings = NULL, bertopic_kwargs = list()) {
      list(topic_ids = c(11L, 12L), model = "model", fit_raw = "fit")
    },
    build_bertopic_hierarchy = function(questions, question_embeddings, topic_levels = NULL) {
      list(
        questions = questions,
        assignments = fake_state$assignments,
        clusters = fake_state$clusters,
        clusters_by_level = fake_state$clusters_by_level
      )
    },
    save_tag_state = function(state, path) invisible(NULL),
    tag_clusters_bottom_up = function(state, sample_size, progress_path, model, base_url) state,
    build_question_tag_matrix = function(assignments, clusters) tibble::tibble(id = assignments$id, demo_age = c(1L, 0L)),
    clean_tag_matrix = function(tag_matrix) list(tag_matrix_clean = tag_matrix, dropped_tags = character()),
    build_tag_registry_from_clean = function(tag_matrix_clean) tibble::tibble(tag = "demo_age"),
    embed_tag_registry = function(tag_registry, model, base_url) matrix(1, nrow = nrow(tag_registry), ncol = 2),
    find_synonym_candidates_embeddings = function(tag_registry, tag_emb, min_sim, auto_sim) tibble::tibble(),
    build_synonym_group_candidates = function(cands, tag_registry) tibble::tibble(),
    summarise_synonym_groups = function(group_candidates, cands) tibble::tibble(),
    reorder_tag_matrix_by_support = function(tag_matrix_clean) list(tag_matrix_clean = tag_matrix_clean),
    .env = asNamespace("tagger")
  )

  out <- run_bertopic_tagger_pipeline(forms = tibble::tibble(form = list(list())) )

  expect_named(
    out,
    c(
      "bertopic", "state", "tag_matrix", "clean", "tag_registry", "tag_embeddings",
      "synonym_pairs", "synonym_groups_for_review", "synonym_status",
      "tag_matrix_post_synonyms", "reorder", "tag_matrix_final"
    )
  )
  expect_equal(out$synonym_status, "pending_human_review")
  expect_s3_class(out$state, "tag_state")
  expect_equal(out$tag_matrix_final$demo_age, c(1L, 0L))
})

test_that("run_bertopic_tagger_pipeline errors if progress file does not contain tag_state", {
  tf <- tempfile(fileext = ".rds")
  saveRDS(list(not = "tag_state"), tf)
  on.exit(unlink(tf), add = TRUE)

  testthat::local_mocked_bindings(
    unnest_questions = function(forms, limit_n = Inf) tibble::tibble(id = "q1", caption = "Question"),
    load_tag_state = function(path) readRDS(path),
    .env = asNamespace("tagger")
  )

  expect_error(
    run_bertopic_tagger_pipeline(forms = tibble::tibble(form = list(list())), progress_path = tf),
    "Progress file is not a tag_state object"
  )
})
