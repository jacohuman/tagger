# Demonstration: tagging questions from forms.Rda
#
# This script runs the full hierarchical tagging pipeline in the requested
# functional steps, while persisting progress to disk so the run can resume.

library(GraphDB)

# ---- Global config (safe defaults) ----
config <- ollama_config()

# 1) Read and unnest the forms.Rda file -----------------------------------
forms_path <- "~/Downloads/form.Rda"
progress_path <- "forms_tagger_progress.rds"
if (file.exists(progress_path)) {
  state <- load_tag_state(progress_path)
} else {
  forms <- load_forms(forms_path)
  demo_questions <- unnest_questions(forms, limit_n = 300)
  state <- new_tag_state(demo_questions)
  save_tag_state(state, progress_path)
}

# 2) Embed each question using the Ollama embedder model -------------------
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

# 3) Hierarchical clustering with configurable k ---------------------------
k <- 256
clusters_by_level <- binary_levels(k)

if (is.null(state$hclust) || is.null(state$assignments) || is.null(state$clusters)) {
  clustering <- cluster_embeddings(state$embeddings, clusters_by_level)
  state$hclust <- clustering$hclust
  state$assignments <- add_cluster_assignments(state$questions, state$hclust, clusters_by_level)
  state$clusters <- build_cluster_index(state$assignments, clusters_by_level)
  save_tag_state(state, progress_path)
}

# 4) Tag deepest clusters, then roll-up with child tags & samples ----------
if (any(is.na(state$clusters$tag)) || any(state$clusters$tag == "untagged")) {
  state <- tag_clusters_bottom_up(
    state = state,
    sample_size = 5,
    progress_path = progress_path,
    model = config$tagger_model,
    base_url = config$base_url
  )
}

# 5) Build question-tag matrix --------------------------------------------
if (is.null(state$tag_matrix)) {
  state$tag_matrix <- build_question_tag_matrix(state$assignments, state$clusters)
  save_tag_state(state, progress_path)
}

# 6) Clean and audit tags --------------------------------------------------
if (is.null(state$cleaned)) {
  cleaned <- clean_tag_hierarchy(
    state$tag_matrix,
    levels = 1:6,
    model = config$tagger_model,
    base_url = config$base_url,
    use_llm = FALSE
  )
  state$tag_matrix <- cleaned$cleaned_matrix
  state$cleaned <- cleaned$mapping
}

if (is.null(state$audit)) {
  state <- audit_tag_paths(state, allow_manual = FALSE)
}

save_tag_state(state, progress_path)

state$tag_matrix
state$audit
