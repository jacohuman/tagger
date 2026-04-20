test_that("prune_tag_from_state removes matching tag assignments and rebuilds matrix", {
  state <- structure(
    list(
      assignments = tibble::tibble(
        id = c("q1", "q2", "q3"),
        caption = c("Age", "Income", "Education"),
        cluster_level_1 = c(1L, 2L, 2L),
        cluster_level_2 = c(1L, 1L, 1L)
      ),
      clusters = tibble::tibble(
        level = c(1L, 1L, 2L),
        cluster_id = c(1L, 2L, 1L),
        parent_cluster = c(1L, 1L, NA_integer_),
        question_ids = list("q1", c("q2", "q3"), c("q1", "q2", "q3")),
        tag = c("age", "income", "demographics")
      )
    ),
    class = "tag_state"
  )

  out <- prune_tag_from_state(state, tag = "income")

  expect_true(is.na(out$assignments$cluster_level_1[out$assignments$id == "q2"]))
  expect_true(is.na(out$assignments$cluster_level_1[out$assignments$id == "q3"]))
  expect_true("tag_level_1" %in% names(out$tag_matrix))
  expect_true("tag_level_2" %in% names(out$tag_matrix))
})

test_that("validate_hierarchy_integrity flags orphaned lower-level assignments", {
  state <- structure(
    list(
      assignments = tibble::tibble(
        id = c("q1", "q2"),
        caption = c("Age", "Income"),
        cluster_level_1 = c(1L, 2L),
        cluster_level_2 = c(1L, NA_integer_)
      ),
      clusters = tibble::tibble(
        level = c(1L, 1L, 2L),
        cluster_id = c(1L, 2L, 1L),
        parent_cluster = c(1L, NA_integer_, NA_integer_),
        question_ids = list("q1", "q2", "q1"),
        tag = c("age", "income", "demo")
      )
    ),
    class = "tag_state"
  )

  info <- validate_hierarchy_integrity(state)
  expect_equal(info$n_questions, 2L)
  expect_equal(info$n_levels, 2L)
  expect_equal(info$orphan_counts$orphan_questions[[1]], 1L)
})


test_that("refine_leaf_cluster_with_bertopic inserts a new leaf level", {
  state <- structure(
    list(
      assignments = tibble::tibble(
        id = c("q1", "q2", "q3"),
        caption = c("Age", "Income", "Education"),
        cluster_level_1 = c(1L, 1L, 2L),
        cluster_level_2 = c(1L, 1L, 1L)
      ),
      clusters = tibble::tibble(
        level = c(1L, 1L, 2L),
        cluster_id = c(1L, 2L, 1L),
        parent_cluster = c(1L, 1L, NA_integer_),
        question_ids = list(c("q1", "q2"), "q3", c("q1", "q2", "q3")),
        tag = c("broad_demo", "education", "all_questions")
      ),
      embeddings = matrix(c(0.1, 0.2, 0.3, 0.4, 0.3, 0.1), nrow = 3, byrow = TRUE)
    ),
    class = "tag_state"
  )

  local_mocked_bindings(
    fit_bertopic_topics = function(texts, embeddings = NULL, bertopic_kwargs = list()) {
      list(topic_ids = c(0L, 1L))
    },
    build_bertopic_hierarchy = function(questions, question_embeddings, topic_levels = NULL) {
      list(
        questions = questions,
        assignments = tibble::tibble(
          id = questions$id,
          caption = questions$caption,
          cluster_level_1 = c(1L, 2L)
        ),
        clusters = tibble::tibble(
          level = 1L,
          cluster_id = c(1L, 2L),
          parent_cluster = NA_integer_,
          question_ids = list("q1", "q2"),
          tag = c(NA_character_, NA_character_)
        )
      )
    },
    tag_clusters_bottom_up = function(state, sample_size, progress_path, model, base_url) {
      state$clusters$tag <- c("age_specific", "income_specific")
      state
    },
    .env = asNamespace("tagger")
  )

  out <- refine_leaf_cluster_with_bertopic(state, cluster_id = 1L)

  expect_true("cluster_level_3" %in% names(out$assignments))
  expect_true("tag_level_1" %in% names(out$tag_matrix))
  expect_true(any(out$clusters$tag == "age_specific"))
})
