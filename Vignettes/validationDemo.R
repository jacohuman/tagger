res_val <- run_tag_path_validation(
  tag_matrix_clean = state$tag_matrix_clean,
  model = DEFAULT_EMBED_MODEL,
  base_url = DEFAULT_OLLAMA_BASE,
  timeout_sec = 60,
  min_tag_sim = 0.30,
  min_prefix_sim = 0.40,
  max_negative_delta = -0.05
)

res_val$pair_level %>%
  dplyr::filter(suspicious) %>%
  dplyr::select(id, caption, level, tag, tag_sim, prefix_sim, prefix_delta, suspicion_reason)

res_val$path_level %>%
  dplyr::filter(suspicious_path)
