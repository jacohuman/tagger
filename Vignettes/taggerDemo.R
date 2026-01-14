# Demonstration: tagging questions from forms.Rda
#
# This script runs the full hierarchical tagging pipeline for 100 survey
# questions. It explicitly follows the requested steps: unnest forms.Rda,
# embed each caption with the Ollama embedding model, cluster at a configurable
# level k, tag from the deepest clusters upward, and roll up tags while keeping
# progress in case the run aborts.

library(GraphDB)
library(tidyverse)
library(httr2)


# ---- Global config (safe defaults) ----
EMBED_MODEL <- Sys.getenv("EMBED_MODEL", unset = "nomic-embed-text")
TAGGER_MODEL <- Sys.getenv("TAGGER_MODEL", unset = "llama3.1:8b")

# Support both OLLAMA_BASE and OLLAMA_BASE_URL env vars
OLLAMA_BASE <- Sys.getenv(
  "OLLAMA_BASE",
  unset = Sys.getenv("OLLAMA_BASE_URL", unset = "http://localhost:11434")
)

# 1) Read and unnest the forms.Rda file -----------------------------------
forms_path <- "~/Downloads/form.Rda"
stopifnot(file.exists(forms_path))
load(forms_path)
stopifnot(exists("forms"))

forms_flat <- forms %>%
  unnest(form) %>%
  filter(!is.na(caption), nzchar(caption))

# Keep a consistent 100-question slice for the demo
demo_questions <- forms_flat %>%
  distinct(caption) %>%
  slice_head(n = 300)

demo_forms <- tibble(form = list(demo_questions))

# 2) Embed each question using the Ollama embedder model -------------------
# The embedding happens inside run_question_tagger(); we preview what will be
# embedded to ensure the captions look correct.
print(demo_questions %>% slice_head(n = 5))

# 3) Hierarchical clustering with configurable k ---------------------------
# Choose the bottom-level cluster count (k) and optionally additional levels.
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

# 5) Inspect the rolled-up tags -------------------------------------------
tagged$clusters %>%
  select(level, cluster_id, parent_cluster, tag) %>%
  arrange(level, cluster_id) %>%
  print(n = Inf)

# 6) Map tagged clusters back to the sample questions ----------------------
tagged$assignments %>%
  select(id, caption, cluster_level_1) %>%
  left_join(
    tagged$clusters %>% filter(level == 1) %>% select(cluster_id, tag),
    by = c("cluster_level_1" = "cluster_id")
  ) %>%
  arrange(cluster_level_1)



#############################################


tag_lookup <- tagged$clusters %>%
  select(level, cluster_id, tag)

questions_by_level <- tagged$assignments %>%
  # cluster_level_1, cluster_level_2, ...
  pivot_longer(
    cols = starts_with("cluster_level_"),
    names_to = "level_name",
    values_to = "cluster_id"
  ) %>%
  mutate(
    # "cluster_level_1" -> 1, etc.
    level = as.integer(str_remove(level_name, "cluster_level_"))
  ) %>%
  left_join(tag_lookup, by = c("level", "cluster_id")) %>%
  arrange(id, level)

questions_by_level

question_tag_matrix <- questions_by_level %>%
  select(id, caption, level, tag) %>%
  mutate(level_name = paste0("tag_level_", level)) %>%
  select(-level) %>%
  distinct() %>%
  pivot_wider(
    names_from = level_name,
    values_from = tag
  ) %>%
  arrange(id)

question_tag_matrix
