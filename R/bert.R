# bertopic_demo.R
# ------------------------------------------------------------
# End-to-end BERTopic demo from R using bertopicr + reticulate
# ------------------------------------------------------------

# ---------------------------
# 0) Install/load R packages
# ---------------------------
# Run these once if needed:
# install.packages("bertopicr")
# install.packages(c("reticulate", "dplyr", "tibble", "readr"))

library(bertopicr)
library(reticulate)
library(dplyr)
library(tibble)

# ---------------------------
# 1) Python environment setup
# ---------------------------
# bertopicr provides a helper for creating/configuring the Python env. :contentReference[oaicite:2]{index=2}
# Change method = "conda" if you prefer conda.
setup_python_environment(
  envname = "r-bertopic",
  method = "virtualenv"
)

# Point reticulate at that environment
use_virtualenv("r-bertopic", required = TRUE)
py_config()

# Confirm important Python modules are available
cat("bertopic available:            ", py_module_available("bertopic"), "\n")
cat("sentence_transformers available:", py_module_available("sentence_transformers"), "\n")
cat("umap available:                ", py_module_available("umap"), "\n")
cat("hdbscan available:             ", py_module_available("hdbscan"), "\n")
cat("sklearn available:             ", py_module_available("sklearn"), "\n")

# ------------------------------------------------
# 2) Stability / threading settings (recommended)
# ------------------------------------------------
Sys.setenv(
  TOKENIZERS_PARALLELISM = "false",
  OMP_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  PYTORCH_ENABLE_MPS_FALLBACK = "1"
)

# ------------------------------------------
# 3) Provide your texts here
# ------------------------------------------
# Option A: from your existing object
texts <- state[["questions"]][["caption"]]

# Option B: fallback toy demo if `state` doesn't exist
if (!exists("texts")) {
  texts <- c(
    "Please capture the household GPS location",
    "Please select the type of roof material",
    "How many people live in this household",
    "Record the stand number of the property",
    "Please scan the QR code on the device",
    "Please capture the water source used by the household",
    "Does the household have access to electricity",
    "Please explain the waste disposal method used",
    "What is the main building material of the dwelling",
    "Please select the sanitation facility type",
    "Please scan the ibutton attached to the device",
    "How many rooms are used for sleeping"
  )
}

# Clean minimal issues
texts <- as.character(texts)
texts <- texts[!is.na(texts)]
texts <- trimws(texts)
texts <- texts[nzchar(texts)]

cat("Number of documents:", length(texts), "\n")

# Save raw texts for reference
writeLines(texts, "texts_input.txt")

# ---------------------------------------------------------
# 4) SIMPLE helper-based BERTopic workflow via bertopicr
# ---------------------------------------------------------
# This is the easiest end-to-end path from R. bertopicr provides
# train/save/load/visualize helpers. :contentReference[oaicite:3]{index=3}

cat("\n=== SIMPLE helper-based BERTopic run ===\n")

topic_model_helper <- train_bertopic_model(
  docs = texts,
  embedding_model = "Qwen/Qwen3-Embedding-0.6B",
  embedding_batch_size = 8L,
  calculate_probabilities = FALSE,
  compute_reduced_embeddings = FALSE,
  compute_hierarchical_topics = FALSE,
  top_n_words = 3L,
  verbose = TRUE
)

# Topic assigned to each caption/document
doc_topics_helper <- topic_model_helper$topics

# Topic summary
topic_info_helper <- get_topic_info_df(model = topic_model_helper$model)
print(topic_info_helper)

# Save and reload
save_bertopic_model(topic_model_helper, "topic_model_helper")

loaded_helper <- load_bertopic_model("topic_model_helper")
model_helper <- loaded_helper$model
probs_helper <- loaded_helper$extras$probabilities

# Visualizations from helper model
# Note: visualize_distribution requires probabilities; if probabilities
# were not calculated, this may be NULL / unavailable.
visualize_topics(
  model = model_helper,
  filename = "helper_intertopic_distance_map",
  auto_open = FALSE
)

if (!is.null(probs_helper)) {
  visualize_distribution(
    model = model_helper,
    text_id = 1,
    probabilities = probs_helper,
    auto_open = FALSE
  )
}

# Join helper topics back to captions
helper_caption_topics <- tibble(
  caption_id = seq_along(texts),
  caption = texts,
  topic = doc_topics_helper
)

write.csv(helper_caption_topics, "helper_caption_topics.csv", row.names = FALSE)

# ----------------------------------------------------------------
# 5) MANUAL workflow via reticulate (more control, closer to Python)
# ----------------------------------------------------------------
# This mirrors the BERTopic pipeline:
# embeddings -> UMAP -> HDBSCAN -> BERTopic -> topic info
# BERTopic docs describe this pipeline and optional representation
# refinement on top of c-TF-IDF. :contentReference[oaicite:4]{index=4}

cat("\n=== MANUAL BERTopic run via reticulate ===\n")

# Import Python modules
bertopic <- import("bertopic")
umap <- import("umap")
hdbscan <- import("hdbscan")
st <- import("sentence_transformers")
sk <- import("sklearn")

CountVectorizer <- sk$feature_extraction$text$CountVectorizer

# -------------------------------------------------
# 6) Stopwords + vectorizer model
# -------------------------------------------------
# Good for survey captions, especially boilerplate words
stopwords <- c(
  "the","a","an","and","or","to","of","in","on","for","with","is","are",
  "you","your","we","our","this","that","please","capture","select",
  "other","explain","record","enter","specify"
)

vectorizer_model <- CountVectorizer(
  stop_words   = stopwords,
  ngram_range  = reticulate::tuple(1L, 3L),
  min_df       = 2L,
  max_features = 20000L
)

# -------------------------------------------------
# 7) Embeddings
# -------------------------------------------------
# BERTopic can compute embeddings internally, but explicit embedding
# generation often gives you more control and avoids some instability. :contentReference[oaicite:5]{index=5}
embedding_model <- st$SentenceTransformer("Qwen/Qwen3-Embedding-0.6B")

embeddings <- embedding_model$encode(
  texts,
  show_progress_bar = TRUE,
  batch_size = 8L
)

# -------------------------------------------------
# 8) UMAP + HDBSCAN
# -------------------------------------------------
umap_model <- umap$UMAP(
  n_neighbors = 10L,
  n_components = 5L,
  min_dist = 0.0,
  metric = "cosine",
  random_state = 42L
)

hdbscan_model <- hdbscan$HDBSCAN(
  min_cluster_size = 10L,
  min_samples = 5L,
  prediction_data = TRUE,
  core_dist_n_jobs = 1L
)

# -------------------------------------------------
# 9) OPTIONAL representation model
# -------------------------------------------------
# BERTopic supports optional representation models for refining topic
# representations beyond default c-TF-IDF keywords. :contentReference[oaicite:6]{index=6}
representation <- import("bertopic.representation")
rep_model <- representation$KeyBERTInspired()

topic_model_py <- bertopic$BERTopic(
  embedding_model = embedding_model,
  umap_model = umap_model,
  hdbscan_model = hdbscan_model,
  vectorizer_model = vectorizer_model,
  representation_model = rep_model,
  top_n_words = 10L,
  calculate_probabilities = FALSE,
  verbose = TRUE
)

if (use_representation_model) {
  rep_model <- tryCatch(
    representation$KeyBERTInspired(),
    error = function(e) {
      message("Could not create KeyBERTInspired representation model. Proceeding without it.")
      NULL
    }
  )
}

# -------------------------------------------------
# 10) BERTopic model
# -------------------------------------------------
if (is.null(rep_model)) {
  topic_model_py <- bertopic$BERTopic(
    umap_model = umap_model,
    hdbscan_model = hdbscan_model,
    vectorizer_model = vectorizer_model,
    top_n_words = 10L,
    calculate_probabilities = FALSE,
    verbose = TRUE
  )
} else {
  topic_model_py <- bertopic$BERTopic(
    umap_model = umap_model,
    hdbscan_model = hdbscan_model,
    vectorizer_model = vectorizer_model,
    representation_model = rep_model,
    top_n_words = 10L,
    calculate_probabilities = FALSE,
    verbose = TRUE
  )
}

# -------------------------------------------------
# 11) Fit model
# -------------------------------------------------
fit <- topic_model_py$fit_transform(texts, embeddings)

# fit is typically a Python tuple: (topics, probs)
topics <- fit[[1]]
probs  <- NULL
if (length(fit) >= 2) {
  probs <- fit[[2]]
}

# -------------------------------------------------
# 12) Extract topic info
# -------------------------------------------------
# bertopicr helpers are handy for turning BERTopic output into data frames
topic_info_df <- get_topic_info_df(
  model = topic_model_py,
  drop_expanded_columns = TRUE
)

topics_df <- get_topics_df(model = topic_model_py)

cat("\n--- Topic summary ---\n")
print(topic_info_df)

cat("\n--- Topic terms ---\n")
print(topics_df)

write.csv(topic_info_df, "topic_info_df.csv", row.names = FALSE)
write.csv(topics_df, "topics_df.csv", row.names = FALSE)

# -------------------------------------------------
# 13) Match caption to assigned topic
# -------------------------------------------------
caption_topics <- tibble(
  caption_id = seq_along(texts),
  caption = texts,
  topic = as.integer(topics)
) %>%
  left_join(
    topic_info_df %>% select(Topic, Name, Count),
    by = c("topic" = "Topic")
  )

cat("\n--- Caption-topic assignments ---\n")
print(caption_topics)

write.csv(caption_topics, "caption_topics.csv", row.names = FALSE)

# Example: inspect one non-outlier topic if available
candidate_topics <- setdiff(unique(caption_topics$topic), -1L)
if (length(candidate_topics) > 0) {
  chosen_topic <- candidate_topics[[1]]
  cat("\n--- Example documents for topic", chosen_topic, "---\n")
  print(
    caption_topics %>%
      filter(topic == chosen_topic) %>%
      select(caption_id, caption, Name) %>%
      slice_head(n = 30)
  )
}

# -------------------------------------------------
# 14) Visualisations
# -------------------------------------------------
# bertopicr exposes helper visualization wrappers. The manual already
# documents several such functions. :contentReference[oaicite:7]{index=7}

# Intertopic distance map
visualize_topics(
  model = topic_model_py,
  filename = "manual_intertopic_distance_map",
  auto_open = FALSE
)

# Similarity heatmap
visualize_heatmap(
  model = topic_model_py,
  filename = "topics_similarity_heatmap",
  auto_open = FALSE
)

# Hierarchy
# BERTopic supports hierarchical topic analysis after fitting.
hier_topics <- topic_model_py$hierarchical_topics(texts)

visualize_hierarchy(
  model = topic_model_py,
  hierarchical_topics = hier_topics,
  filename = "topic_hierarchy",
  auto_open = FALSE
)

# Optional document visualization if reduced embeddings exist / are computed separately
# Some bertopicr visualization helpers require reduced embeddings.
# If you want 2D doc plots, you can create reduced embeddings yourself.

# -------------------------------------------------
# 15) Optional: update topic representation after training
# -------------------------------------------------
# BERTopic docs note that topic representations can be updated after
# training, e.g. when changing stop words / n-grams. :contentReference[oaicite:8]{index=8}
update_after_training <- FALSE

if (update_after_training) {
  vectorizer_model_2 <- CountVectorizer(
    stop_words = stopwords,
    ngram_range = reticulate::tuple(1L, 2L),
    min_df = 2L,
    max_features = 10000L
  )

  topic_model_py$update_topics(
    texts,
    vectorizer_model = vectorizer_model_2
  )

  updated_topic_info_df <- get_topic_info_df(
    model = topic_model_py,
    drop_expanded_columns = TRUE
  )

  write.csv(updated_topic_info_df, "updated_topic_info_df.csv", row.names = FALSE)
  cat("\n--- Updated topic summary ---\n")
  print(updated_topic_info_df)
}

# -------------------------------------------------
# 16) Save useful outputs
# -------------------------------------------------
saveRDS(caption_topics, "caption_topics.rds")
saveRDS(topic_info_df, "topic_info_df.rds")
saveRDS(topics_df, "topics_df.rds")

cat("\nDone.\n")
cat("Created files include:\n")
cat("- texts_input.txt\n")
cat("- helper_caption_topics.csv\n")
cat("- topic_info_df.csv\n")
cat("- topics_df.csv\n")
cat("- caption_topics.csv\n")
cat("- manual_intertopic_distance_map.html\n")
cat("- topics_similarity_heatmap.html\n")
cat("- topic_hierarchy.html\n")
