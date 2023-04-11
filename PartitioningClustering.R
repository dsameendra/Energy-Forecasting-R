library(readxl)
library(dplyr)
library(outliers)
library(cluster)
library(factoextra)
library(NbClust)

# Loading the dataset
df <- read_excel("vehicles.xlsx")
df

# Select only the first 18 attributes for clustering
df <- df[, 1:18]
df

# Removing missing values
df <- na.omit(df)
df

# Scaling data using z-score normalization
df_scaled <- scale(df)
df

mahalanobis_outliers <- function(data) {
  cov_matrix <- cov(data)
  inv_cov_matrix <- solve(cov_matrix)
  mahalanobis_dist <- mahalanobis(data, colMeans(data), inv_cov_matrix)
  cutoff <- quantile(mahalanobis_dist, 0.95)
  outliers <- which(mahalanobis_dist > cutoff)
  return(outliers)
}

# Detecting outliers using Mahalanobis distance
outliers <- mahalanobis_outliers(df_scaled)
outliers

# Removing outliers from the dataset
df_no_outliers <- df[-outliers, ]
scaled_df_no_outliers <- df_scaled[-outliers, ]
scaled_df_no_outliers

# Automated tools for determining number of cluster centers
# NBclust
nbclust_result <- NbClust(scaled_df_no_outliers, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nbclust_clusters <- nbclust_result$Best.nc[1]
print(nbclust_clusters)

# Elbow method
elbow_result <- fviz_nbclust(scaled_data, kmeans, method = "wss", k.max = 10)
elbow_clusters <- elbow_result$nb[1]

# Gap statistics
gap_result <- clusGap(scaled_data, FUNcluster = kmeans, nstart = 10, K.max = 10, B = 50)
gap_clusters <- maxSE(gap_result, method="Tibs2001SEmax")$Cluster[1]

# Silhouette method
silhouette_result <- silhouette_samples(scaled_data, kmeans(nbclust_clusters, scaled_data)$cluster)
silhouette_clusters <- table(silhouette_result)$Cluster[1]

# Perform k-means clustering with the most favoured k from automated tools
k <- nbclust_clusters
kmeans_result <- kmeans(scaled_data, centers = k)

# Calculate the between_cluster_sums_of_squares (BSS) and within_cluster_sums_of_squares (WSS) indices
BSS <- sum(kmeans_result$betweenss)
WSS <- sum(kmeans_result$tot.withinss)

# Calculate ratio of BSS over TSS
TSS <- BSS + WSS
BSS_ratio <- BSS / TSS

# Silhouette plot
silhouette_plot <- silhouette_plot(scaled_data, kmeans_result$cluster)
avg_silhouette_width <- mean(silhouette_result[, "sil_width"])

# Print results
cat("Number of clusters determined by NBclust: ", nbclust_clusters, "\n")
cat("Number of clusters determined by Elbow method: ", elbow_clusters, "\n")
cat("Number of clusters determined by Gap statistics: ", gap_clusters, "\n")
cat("Number of clusters determined by Silhouette method: ", silhouette_clusters, "\n")
cat("K-means clustering results: \n")
cat("Cluster Centers: \n", kmeans_result$centers, "\n")
cat("Between Cluster Sum of Squares (BSS): ", BSS, "\n")
cat("Within Cluster Sum of Squares (WSS): ", WSS, "\n")
cat("BSS/TSS Ratio: ", BSS_ratio, "\n")
cat("Average Silhouette Width: ", avg_silhouette_width, "\n")

# Plot Silhouette plot
plot(silhouette_plot)