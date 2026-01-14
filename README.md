# Survey Data Extraction to JSON-LD R Package

## Overview

GraphDB provides helpers to:

- import survey data from CSV files and normalise survey identifiers,
- embed and cluster question captions using local Ollama models, and
- export survey content to JSON-LD aligned to the [Survey Ontology](https://w3id.org/survey-ontology),
- tag survey questions with a resumable, functional pipeline.



## Tagging Pipeline

### Starting out

The Ollama endpoint and models to use for embedding and tagging needs to be specified.
The defaults can be overridden with the following environment variables:

```
export OLLAMA_BASE_URL="http://localhost:11434"
export EMBED_MODEL="nomic-embed-text"
export TAGGER_MODEL="llama3.1:8b"
```

To configure the parameters for embedding and tagging use the `ollama_config()`
function as follows:

```
conf <- ollama_config()
# This configures the parameters to their default values or the environment variables (if set)

# Alternatively override any parameters in the function call aas follows
conf <- ollama_config(base_url = "http://localhost:11434")
```

### Functional pipeline

The tagging pipeline is designed as a sequence of small, reusable functions
that can be resumed from disk. The full pipeline is wrapped by
`run_tagger_pipeline()`, but each step is also exposed for manual execution.

Major functions and what they do:

- `ollama_config()`: collect Ollama base URL and model names.
- `load_forms()`: read a `.Rda` forms tibble from disk.
- `unnest_questions()`: extract unique question captions.
- `embed_multiple_questions()`: call the Ollama embedder for all captions.
- `cluster_embeddings()` + `add_cluster_assignments()`: build hierarchical
  clusters from embeddings.
- `build_cluster_index()`: index clusters and parent/child relationships.
- `tag_clusters_bottom_up()`: tag clusters, saving state after each tag.
- `build_question_tag_matrix()`: map tags to each question across levels.
- `clean_tag_hierarchy()` + `audit_tag_paths()`: clean tag hierarchy and flag
  redundant/missing tags.

Example end-to-end run:

```r
conf <- ollama_config()
state <- run_tagger_pipeline(
  forms_path = "path/to/forms.Rda",
  clusters_by_level = binary_levels(128),
  progress_path = "tagger_state.rds",
  config = conf,
  use_llm_cleaning = FALSE
)

state$tag_matrix
state$audit
```

### Embeddings

Question captions are sent to the configured Ollama embedding model and the
corresponding vector embeddings are returned.  These embeddings are then used
for semantic hierarchical clustering.

The `embedding.R` file contains the methods for embedding the survey questions.
Each question's text is sent to the Ollama embedding model as a HTTP request and
the corresponding embedding vector gets returned.

As a preliminary step the R data frame (containing the survey data) needs to be un-nested
so the question captions can be sent to the embedder.
```
forms_path <- "...pathTo/form.Rda"
forms <- load_forms(forms_path)
forms_flat <- unnest_questions(forms)
```

Once flattened the question captions are sent to the embedding model:
```
embeddings <- embed_multiple_questions(
  forms_flat$caption,
  model = conf$embed_model,
  base_url = conf$base_url
)
```

### Clustering

Hierarchical Clustering is an unsupervised learning method used to group similar data points into clusters based 
on their distance or similarity. Instead of choosing the number of clusters in advance, it builds a tree-like structure 
called a dendrogram that shows how clusters merge or split at different levels. 
It helps identify natural groupings indata and is commonly used in pattern recognition, customer segmentation, gene analysis and image grouping.
It builds a multilevel hierarchy of clusters by either merging smaller clusters into larger ones (agglomerative) or 
dividing a large cluster into smaller ones (divisive). This results in a tree-like structure known as a dendrogram.

## Development workflow

1. Install dependencies and regenerate documentation:

   ```r
   devtools::document()
   ```

2. Build and check the package:

   ```r
   devtools::build()
   ```

3. Run the forms tagging demonstration once you have `forms.Rda` available:

   ```r
   source("Vignettes/taggerDemo.R")
   ```

## Question tagging with Ollama

For local development, install and start Ollama:

```sh
brew install ollama
ollama serve
ollama pull llama3.1:8b
ollama pull nomic-embed-text
```
