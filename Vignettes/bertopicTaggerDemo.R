# bertopicTaggerDemo.R
# ------------------------------------------------------------------
# Demo: BERTopic + existing tagger cleanup/audit pipeline
# ------------------------------------------------------------------
# This demo exercises `run_bertopic_tagger_pipeline()` from
# R/bertopic_tagger_pipeline.R.

library(dplyr)

# ------------------------------------------------------------------
# 0) Load forms data
# ------------------------------------------------------------------
# Option A: use an existing forms object in memory
# forms <- forms
#
# Option B: load from file
# load("forms.Rda")
# (expects object named `forms`)

stopifnot(exists("forms"))

# ------------------------------------------------------------------
# 1) Configure models and endpoint
# ------------------------------------------------------------------
cfg <- ollama_config()

# Optional overrides
# cfg$base_url <- "http://localhost:11434"
# cfg$embed_model <- "nomic-embed-text"
# cfg$tagger_model <- "llama3.1:8b"

# ------------------------------------------------------------------
# 2) Run BERTopic pipeline (without synonym collapse first)
# ------------------------------------------------------------------
# NOTE:
# - This run returns synonym candidates for human review in
#   `$synonym_groups_for_review`.
# - Because `reviewed_groups = NULL`, no synonym collapse is applied yet.

set.seed(42)

res_stage1 <- run_bertopic_tagger_pipeline(
  forms = forms,
  limit_n = 300,
  sample_size = 6,
  max_tag_rounds = 7,
  stable_streak = 2,
  progress_path = "bertopic_pipeline_stage1.rds",
  reviewed_groups = NULL,
  min_synonym_sim = 0.88,
  auto_synonym_sim = 0.94,
  config = cfg,
  bertopic_kwargs = list(
    verbose = TRUE,
    calculate_probabilities = FALSE
  )
)

# Quick inspection
print(res_stage1$synonym_status)
print(head(res_stage1$tag_matrix_final, 10))
print(head(res_stage1$synonym_pairs, 20))
print(head(res_stage1$synonym_groups_for_review, 30))

# ------------------------------------------------------------------
# 3) Human review step for synonym groups
# ------------------------------------------------------------------
# Export candidate groups, review manually, then fill:
# - approved_group (TRUE/FALSE)
# - canonical_tag   (single canonical per approved group)

review_sheet <- res_stage1$synonym_groups_for_review
write.csv(review_sheet, "synonym_groups_for_review.csv", row.names = FALSE)

# ---- Example only (replace with your reviewed sheet):
# reviewed_groups <- read.csv("synonym_groups_reviewed.csv", stringsAsFactors = FALSE)

# Minimal no-op example review (approve nothing)
reviewed_groups <- review_sheet %>%
  mutate(
    approved_group = FALSE,
    canonical_tag = suggested_canonical
  )

# ------------------------------------------------------------------
# 4) Re-run with reviewed groups to apply synonym collapse
# ------------------------------------------------------------------
res_stage2 <- run_bertopic_tagger_pipeline(
  forms = forms,
  limit_n = 300,
  sample_size = 6,
  max_tag_rounds = 7,
  stable_streak = 2,
  progress_path = "bertopic_pipeline_stage2.rds",
  reviewed_groups = reviewed_groups,
  min_synonym_sim = 0.88,
  auto_synonym_sim = 0.94,
  config = cfg,
  bertopic_kwargs = list(
    verbose = TRUE,
    calculate_probabilities = FALSE
  )
)

# ------------------------------------------------------------------
# 5) Outputs to inspect
# ------------------------------------------------------------------
# Final matrix after cleanup + synonym collapse + reorder (general -> specific)
final_tags <- res_stage2$tag_matrix_final

# Optional deterministic audit report
audit_df <- final_tags %>%
  select(id, caption) %>%
  mutate(
    after_path = purrr::pmap_chr(
      select(final_tags, matches("^tag_level_")),
      ~ paste(stats::na.omit(c(...)), collapse = " > ")
    )
  )

audit_det <- audit_report_deterministic(audit_df)

print(head(final_tags, 15))
print(head(audit_det$tag_registry, 20))
print(head(audit_det$hierarchy_violations, 20))

# Save final outputs
write.csv(final_tags, "bertopic_tag_matrix_final.csv", row.names = FALSE)
write.csv(audit_det$tag_registry, "bertopic_tag_registry.csv", row.names = FALSE)
write.csv(audit_det$hierarchy_violations, "bertopic_hierarchy_violations.csv", row.names = FALSE)
