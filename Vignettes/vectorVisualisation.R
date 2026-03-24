# ---- 2) Embed  ----
emb_list <- embed_many(unique_q$caption)
emb_mat <- do.call('rbind', emb_list)
df <- as.data.frame(as.table(emb_mat))
colnames(df) <- c("Question", "Dimension", "Value")

ggplot(df, aes(x = Dimension, y = Question, fill = Value)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())



library(factoextra)
library(ggplot2)
fviz_cluster(res.hcpc, geom = "point", pointsize = 1, ellipse = FALSE)

library(ggplot2)

# Pull id/labels and the embedding as text so we can parse it in R
emb_df <- DBI::dbGetQuery(
  con,
  "SELECT id, survey_id, section, tags, embedding::text AS emb_txt
   FROM questions
   WHERE embedding IS NOT NULL"
)

# Parse a pgvector text like "[0.1,0.2,...]" -> numeric vector
parse_pgvector <- function(x) {
  x <- gsub("\\[|\\]", "", x)
  as.numeric(strsplit(x, ",")[[1]])
}

# Build matrix (rows = questions, cols = 768)
emb_mat <- do.call(rbind, lapply(emb_df$emb_txt, parse_pgvector))

# Optional: L2-normalize rows (good for cosine-like geometry)
l2 <- sqrt(rowSums(emb_mat^2))
emb_norm <- emb_mat / pmax(l2, 1e-12)

pca <- prcomp(emb_norm, center = TRUE, scale. = FALSE)
viz_pca <- cbind(
  emb_df[, c("id","survey_id","section")],
  as.data.frame(pca$x[, 1:2])
) %>% rename(PC1 = PC1, PC2 = PC2)

ggplot(viz_pca, aes(PC1, PC2, color = survey_id)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(title = "Embeddings (PCA 2D)", color = "Survey")
