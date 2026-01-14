# Survey Data Extraction to JSON-LD R Package

## Overview

GraphDB provides helpers to:

- import survey data from CSV files and normalise survey identifiers,
- embed and cluster question captions using local Ollama models, and
- export survey content to JSON-LD aligned to the [Survey Ontology](https://w3id.org/survey-ontology).



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
conf <- ollama_config(base_url = http://localhost:11434)
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
load(forms_path)

forms_flat <- form %>%
  unnest(form) %>%
  filter(!is.na(caption), nzchar(caption))
```

Once flattened the question captions are sent to the embedding model:
```
embeddings <- embed_multiple_questions(forms_flat, model = EMBED_MODEL, base = OLLAMA_BASE)
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
   source("Vignettes/formsDemo.R")
   ```

## Question tagging with Ollama

For local development, install and start Ollama:

```sh
brew install ollama
ollama serve
ollama pull llama3.1:8b
ollama pull nomic-embed-text
```


