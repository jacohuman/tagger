library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(writexl)
install.packages("readxl")
library(readxl)

source("tag_cleanup_synonyms.R")

# 1) Deterministic cleaning first
res <- clean_tag_matrix(state$tag_matrix)
cleaned <- res$tag_matrix_clean
audit_clean <- res$audit

# 2) Build tag registry from cleaned tags
tag_registry <- build_tag_registry_from_clean(cleaned)

# 3) Embed tags using Ollama
tag_emb <- embed_tag_registry(
  tag_registry = tag_registry,
  model = DEFAULT_EMBED_MODEL ,
  base_url = DEFAULT_OLLAMA_BASE ,
  timeout_sec = 60
)

# 4) Detect embedding-based near-synonym candidate pairs
cands <- find_synonym_candidates_embeddings(
  tag_registry = tag_registry,
  tag_emb = tag_emb,
  min_sim = 0.88,
  auto_sim = 0.94,
  min_freq = 2
)

# 5) Build provisional synonym groups from the pairwise candidates
group_candidates <- build_synonym_group_candidates(
  cands = cands,
  tag_registry = tag_registry
)

# 6) Add summary columns to help review
group_review_sheet <- summarise_synonym_groups(
  group_candidates = group_candidates,
  cands = cands
)

write_xlsx(
  list(
    synonym_review = group_review_sheet
  ),
  path = "synonym_review.xlsx"
)

# ---- HUMAN REVIEW STEP ----
# Review `group_review_sheet` and fill in:
# - approved_group
# - canonical_tag
#
reviewed_groups <- read_xlsx("synonym_review.xlsx")

# 7) Build final tag map from approved reviewed groups
tag_map <- build_tag_map_from_group_review(reviewed_groups)

# 8) Apply synonym collapse
res_syn <- apply_synonym_collapse(
  tag_matrix_clean = cleaned,
  tag_map = tag_map
)

state$tag_matrix_clean <- res_syn$tag_matrix_clean
audit_syn <- res_syn$audit

# 9) Build final monotonicity input
audit_final <- state$tag_matrix_clean %>%
  dplyr::select(id, caption) %>%
  dplyr::mutate(
    after_path = purrr::pmap_chr(
      dplyr::select(state$tag_matrix_clean, dplyr::matches("^tag_level_")),
      ~ paste(stats::na.omit(c(...)), collapse = " > ")
    )
  )

# Example next step:
# report <- audit_report_deterministic(audit_final, path_col = "after_path")
