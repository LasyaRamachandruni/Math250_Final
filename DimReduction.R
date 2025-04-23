library(dplyr)
library(ggplot2)
library(Rtsne)
library(umap)
head(data)
str(data)
# Helper function to parse a stringified numeric list
parse_list_column <- function(x) {
  x <- gsub("\\[|\\]", "", x)
  x <- gsub(" ", "", x)
  if (nchar(x) == 0) return(NA_real_)
  nums <- as.numeric(unlist(strsplit(x, ",")))
  if (length(nums) == 0 || all(is.na(nums))) return(NA_real_)
  return(nums)
}

# Use the correct column names as shown in your data
list_columns <- c("altitude", "heart_rate", "longitude", "timestamp.x")

for (col in list_columns) {
  parsed <- lapply(data[[col]], parse_list_column)
  data[[paste0(col, "_mean")]] <- vapply(parsed, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE), numeric(1))
  data[[paste0(col, "_sd")]]   <- vapply(parsed, function(x) if (all(is.na(x))) NA_real_ else sd(x, na.rm = TRUE), numeric(1))
  data[[paste0(col, "_min")]]  <- vapply(parsed, function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE), numeric(1))
  data[[paste0(col, "_max")]]  <- vapply(parsed, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE), numeric(1))
}

# Optionally, select only the summary columns for further analysis
summary_cols <- unlist(lapply(list_columns, function(col) {
  paste0(col, c("_mean", "_sd", "_min", "_max"))
}))
features <- data[, summary_cols]

# Inspect the result
head(features)

# Scale the features (recommended for PCA)
features_scaled <- scale(features)

# Run PCA
pca <- prcomp(features_scaled, center = TRUE, scale. = TRUE)

# View variance explained
summary(pca)

# Plot the first two principal components
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", main = "PCA: First Two Principal Components")



# Assume you already have PCA results and your data frame has a 'sport' column
features_scaled <- scale(features)
pca <- prcomp(features_scaled)
pca_df <- as.data.frame(pca$x[, 1:2])  # First two PCs
pca_df$sport <- data$sport             # Add sport info

# Plot with color
ggplot(pca_df, aes(x = PC1, y = PC2, color = sport)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "PCA: First Two Principal Components Colored by Sport")

pca_df$gender <- data$gender

ggplot(pca_df, aes(x = PC1, y = PC2, color = gender)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "PCA: First Two Principal Components Colored by Gender")

set.seed(42)
km <- kmeans(features_scaled, centers = 3)
pca_df$cluster <- as.factor(km$cluster)

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "PCA: First Two Principal Components Colored by Cluster")

#t-SNE

# Remove duplicate rows
features_nodup <- features_scaled[!duplicated(features_scaled), ]

# If you want to keep track of which rows remain, you can also get the indices:
nodup_indices <- which(!duplicated(features_scaled))

sport_nodup <- data$sport[nodup_indices]
gender_nodup <- data$gender[nodup_indices]

# Run t-SNE on deduplicated data
library(Rtsne)
set.seed(42)
tsne_out <- Rtsne(features_nodup, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

# Prepare t-SNE results for plotting
tsne_df <- as.data.frame(tsne_out$Y)
colnames(tsne_df) <- c("TSNE1", "TSNE2")
tsne_df$sport <- sport_nodup
tsne_df$gender <- gender_nodup

library(ggplot2)
ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = sport)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "t-SNE: Workouts Colored by Sport")

library(ggplot2)
ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = gender)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "t-SNE: Workouts Colored by Gender")

set.seed(42)
km <- kmeans(features_scaled, centers = 3)
cluster_nodup <- as.factor(km$cluster[nodup_indices])
tsne_df$cluster <- cluster_nodup
ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "t-SNE: Workouts Colored by Cluster")

#umap

umap_out <- umap(features_nodup)
umap_df <- as.data.frame(umap_out$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$sport <- data$sport[nodup_indices]
umap_df$gender <- data$gender[nodup_indices]
# If you have clusters from k-means (as in your t-SNE step):
umap_df$cluster <- as.factor(km$cluster[nodup_indices])

library(ggplot2)

# By sport
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = sport)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "UMAP: Workouts Colored by Sport")

# By gender
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = gender)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "UMAP: Workouts Colored by Gender")

# By cluster
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "UMAP: Workouts Colored by Cluster")
