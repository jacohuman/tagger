# CONFIG

tagged <- run_question_tagger(
  forms_path
)

k = 256
clusters_by_level <- binary_levels(k)
print(clusters_by_level)

# 4) Tag deepest clusters, then roll-up with child tags & samples ----------
# Progress is written to disk after each cluster; if tagging fails the tag
# will be marked "untagged" and the script will stop so you can resume.
tagged <- run_question_tagger(
  forms = demo_forms,
  clusters_by_level = clusters_by_level,
  limit_n = nrow(demo_questions),
  sample_size = 5,
  progress_path = "forms_tagger_progress.rds",
  embed_model = EMBED_MODEL,
  tag_model = TAGGER_MODEL
)
