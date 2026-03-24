# ======= SETUP =======
library(httr2)
library(dplyr)

################################
#      DEFINE OLLAMA PARAMS
################################

OLLAMA_BASE <- Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
EMBED_MODEL <- Sys.getenv("EMBED_MODEL", "nomic-embed-text")

# NOTE:
# Ensure ollama nomic-embed-text instance is running locally.
# Alternatively use ssh tunnel to connect to the instance running on datadudes2.xyz, using the following terminal command:
#           ssh -fN -o ExitOnForwardFailure=yes -L 11434:127.0.0.1:11434 root@datadudes2.xyz


################################
#      HELPER FUNCTION TO UNNEST FROMS DATA
################################
unnest_questions = function(forms, limit_n) {
  unique_q <- forms %>%
    unnest(form) %>%
    transmute(caption = caption) %>%
    filter(!is.na(caption), nzchar(caption)) %>%
    mutate(caption = stringr::str_squish(caption)) %>%
    distinct(caption) %>%
    slice_head(n = limit_n) %>%
    mutate(id = sprintf("q_%03d", dplyr::row_number()))

  return(unique_q)
}

################################
#      EMBED A SINGLE QUESTION
################################
ollama_embed_one <- function(text, model = EMBED_MODEL) {
  req <- request(paste0(OLLAMA_BASE, "/api/embeddings")) |>
    req_body_json(list(model = model, prompt = text))
  resp <- req_perform(req)
  as.numeric(resp_body_json(resp)$embedding)
}

################################
#      GENERATE EMBEDDINGS
################################
embed_df <- function(df) {
  stopifnot(all(c("id","caption") %in% names(df)))
  emb <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    # Simple progress status
    if (i %% 50 == 0) cat(".. embedded", i, "of", nrow(df), "\n")
    emb[[i]] <- ollama_embed_one(df$caption[[i]])
  }
  df %>% mutate(embedding = emb)
}

################################
#      PCS & HCA
################################
pca_hclust <- function(embeddings_list, max_pcs = 128) {
  E <- do.call(rbind, embeddings_list)
  ncp <- max(2, min(max_pcs, nrow(E) - 1, ncol(E)))
  pca <- prcomp(E, center = TRUE, scale. = TRUE, rank. = ncp)
  X   <- pca$x[, seq_len(ncp), drop = FALSE]
  hc  <- hclust(dist(X), method = "ward.D2")
  hc
}

################################
#      CUT THE CLUSTER TREE
################################
add_cluster_columns <- function(df_with_emb, hc, ks = c(2, 4, 8)) {
  out <- df_with_emb
  for (k in ks) {
    cl <- cutree(hc, k = k)
    # ensure same order: cutree uses original row order; df is unchanged since embed
    out[[paste0("cluster_k", k)]] <- as.integer(cl)
  }
  out
}


################################
#      ADD PATH COLUMN TO DF
################################
add_cluster_path <- function(df, levels = levels_vec) {
  stopifnot(all(levels %in% names(df)))
  df %>%
    rowwise() %>%
    mutate(
      path = paste(paste0("k", gsub("\\D", "", levels), "=",
                          c_across(all_of(levels))), collapse = " / ")
    ) %>%
    ungroup()
}

# ======= example usage =======
# Define the path to the forms.Rda tibble
path <- "/Users/jacohuman/Desktop/Nova_werk/Des_Nov_2025/GraphDB/forms.Rda"
load(path)
df <- unnest_questions(forms = forms, limit_n = 50) # Update limit if you want to embed more questions

df_emb  <- embed_df(df)
hc      <- pca_hclust(df_emb$embedding)
df_out  <- add_cluster_columns(df_emb, hc, ks = c(2,4, 8, 16, 32)) # add desired number of clusters


# choose the cluster levels (as present in df_out)
levels_vec <- c("cluster_k2","cluster_k4","cluster_k8", "cluster_k16", "cluster_k32")  # extend if needed
df_paths <- add_cluster_path(df_out, levels_vec)


################################
#      VISUALISATION
################################
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)


# 1) sort rows by clusters (and id for determinism)
df_sorted <- df_out %>% dplyr::arrange(dplyr::across(all_of(levels_vec)), id)

# 2) extract embeddings as a list in that order
emb_list <- df_sorted$embedding

emb_mat <- do.call('rbind', emb_list)
df <- as.data.frame(as.table(emb_mat))
colnames(df) <- c("Question", "Dimension", "Value")

# pick which level to mark
k_col <- "cluster_k32"   # or "cluster_k2", "cluster_k8", ...

# cluster run-lengths in the SAME order you used for emb_list / df_sorted
cl <- df_sorted[[k_col]]
runs <- rle(cl)

# boundaries are between runs → at cumulative lengths + 0.5 (plot coords)
boundary_lines <- cumsum(runs$lengths) + 0.5

# add separators to your existing ggplot object `p` OR inline after your code:
ggplot(df, aes(x = Dimension, y = Question, fill = Value)) +
  geom_raster() +
  # << lines showing the first/last question of each cluster >>
  geom_hline(yintercept = boundary_lines, color = "black", linewidth = 0.3, inherit.aes = FALSE) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = paste("Embeddings heatmap — cluster boundaries for", k_col))


