# Demonstration: tagging questions from forms.Rda
#
# This script runs the hierarchical tagging pipeline using the wrapper helpers
# so you can resume tagging from the saved progress file.

library(GraphDB)

# ---- Global config (safe defaults) ----
config <- ollama_config()

# 1) Load forms and run the tagging wrapper --------------------------------
forms_path <- "~/Downloads/form.Rda"
progress_path <- "forms_tagger_progress.rds"
forms <- load_forms(forms_path)

k <- 256
clusters_by_level <- binary_levels(k)

state <- run_question_tagger(
  forms = forms,
  clusters_by_level = clusters_by_level,
  limit_n = 300,
  sample_size = 5,
  progress_path = progress_path,
  embed_model = config$embed_model,
  tag_model = config$tagger_model,
  base_url = config$base_url
)

# 2) Build question-tag matrix ---------------------------------------------
if (is.null(state$tag_matrix)) {
  state$tag_matrix <- build_question_tag_matrix(state$assignments, state$clusters)
}

# 3) Clean and audit tags --------------------------------------------------
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
