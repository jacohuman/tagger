# tagger

`tagger` is an R package for survey processing with three major capabilities:

1. **Survey extraction and JSON-LD export** for ontology-aligned publication.
2. **Hierarchical semantic clustering** of question captions.
3. **Hierarchical tag generation** with Ollama (resumable runs and post-cleaning).

It includes two clustering modes:

- **Standard mode**: hierarchical clustering directly on question embeddings (`run_tagger_pipeline()`).
- **BERTopic mode**: BERTopic provides the leaf clusters, then the same bottom-up tagging logic is used (`run_bertopic_tagger_pipeline()`).

---

## Installation

### Development install

```r
# install.packages("devtools")
devtools::install()
```

### Re-generate documentation

```r
devtools::document()
```

This should refresh all `man/*.Rd` files from roxygen comments.

---

## Runtime dependencies

- **R >= 4.2**
- Ollama running locally or remotely
- Required Ollama models (typical):
  - embedding: `nomic-embed-text`
  - tagging: `llama3.1:8b`
- For BERTopic mode only:
  - `reticulate`
  - Python packages: `bertopic`, `sentence-transformers`, `umap-learn`, `hdbscan`, `scikit-learn`

---

## Ollama configuration

You can configure via environment variables:

```sh
export OLLAMA_BASE_URL="http://localhost:11434"
export EMBED_MODEL="nomic-embed-text"
export TAGGER_MODEL="llama3.1:8b"
```

Or in R:

```r
conf <- ollama_config(
  base_url = "http://localhost:11434",
  embed_model = "nomic-embed-text",
  tagger_model = "llama3.1:8b"
)
```

---

## Standard hierarchical tagging workflow

```r
library(tagger)

state <- run_tagger_pipeline(
  forms_path = "path/to/forms.Rda",
  clusters_by_level = binary_levels(128),  # e.g. 128 -> 64 -> ... -> 2
  limit_n = Inf,
  sample_size = 6,
  progress_path = "tagger_state.rds",
  config = ollama_config(),
  use_llm_cleaning = FALSE
)

# Main outputs
state$clusters     # cluster-level tags
state$tag_matrix   # tag_level_1...tag_level_n per question
state$cleaned      # cleanup mappings
state$audit        # hierarchy audit flags
```

### What happens internally

1. `load_forms()` loads forms data.
2. `unnest_questions()` extracts unique captions with stable IDs.
3. `embed_multiple_questions()` gets question embeddings from Ollama.
4. `cluster_embeddings()` + `add_cluster_assignments()` build hierarchy.
5. `build_cluster_index()` creates parent/child metadata.
6. `tag_clusters_bottom_up()` labels leaf to root and checkpoints state.
7. `build_question_tag_matrix()` creates per-question multi-level tags.
8. `clean_tag_hierarchy()` + `audit_tag_paths()` clean and validate.

---

## BERTopic-driven workflow (same tag logic, different clustering)

Use this when you want BERTopic to define leaf topics while preserving the same hierarchical tagging mechanics and parent-tag rollup behavior as the standard pipeline.

```r
library(tagger)

forms <- load_forms("path/to/forms.Rda")

# Optional safety check for Python side
check_bertopic_environment()

res <- run_bertopic_tagger_pipeline(
  forms = forms,
  sample_size = 6,
  progress_path = "bertopic_state.rds",
  topic_levels = NULL,        # auto-infer hierarchy from number of topics
  config = ollama_config(),
  bertopic_kwargs = list(calculate_probabilities = FALSE, verbose = TRUE)
)

res$state$clusters
res$tag_matrix_final
```

### BERTopic mode behavior

- BERTopic assigns each question to a topic (`topic_id`).
- `build_bertopic_hierarchy()` maps topic IDs into leaf cluster IDs.
- Higher levels are formed by clustering topic centroids.
- `tag_clusters_bottom_up()` is reused directly to generate tags from leaf to root.
- Prompt instructions are level-aware (leaf/mid/top-level) to improve reasonable tag generation at each hierarchy level.

---

## Prompting strategy used for tags

Tagging prompts enforce:

- one-word labels
- lowercase ASCII output
- no punctuation/explanations
- cluster-level scope guidance (leaf vs mid-level vs top-level)
- parent labels must be broader than child labels
- avoidance of generic labels and existing hierarchy-path labels

This reduces noisy or overly broad labels while keeping hierarchy semantics consistent.

---

## Resuming interrupted runs

Both pipelines persist state to `.rds` files.

- Re-run with the same `progress_path` to continue.
- Existing tags are reused; only missing/`untagged` clusters are retried.

---

## Build/check/install commands

```r
devtools::document()
devtools::build()
devtools::install()
```

If you are preparing a release, also run:

```r
devtools::check()
```

---

## Notes

- `R/bert.R` contains `check_bertopic_environment()` only (safe helper, no side effects at package load time).
- Example runnable scripts remain under `Vignettes/`.
