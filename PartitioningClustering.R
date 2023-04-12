library(readxl) # for reading Excel files
library(dplyr) # for data manipulation and data wrangling tasks
library(outliers) # for detecting and handling outliers in df
library(cluster) #for clustering
library(NbClust) #for determining optimal number of clusters
library(factoextra) # for visualizing multivariate data analysis results
library(fpc)  # used for calculating Calinski-Harabasz Index
library(ggplot2)  # for data visualization

# Loading the dataset
df <- read_excel("./datasets/vehicles.xlsx")
df

# Select only the 18 attributes for clustering
df <- df[, 2:19]
df

# Removing missing values
df <- na.omit(df)
df

# Outlier removal
z_scores <-
  apply(df, 2, function(x)
    abs(scale(x, center = TRUE, scale = FALSE)))
df_no_outliers <-
  df[rowSums(z_scores < 3) == ncol(df),]

# Scaling data using z-score normalization
scaled_df_no_outliers <- scale(df_no_outliers)
scaled_df_no_outliers

## PART B

# Automated tools for determining number of cluster centers
# NBclust methods

set.seed(42)
nbclust_result <-
  NbClust(
    scaled_df_no_outliers,
    distance = "euclidean",
    min.nc = 2,
    max.nc = 10,
    method = "kmeans",
    index = "all"
  )


# Elbow method
#k = 2:10
#set.seed(42)
#WSS = sapply(k, function(k) {kmeans(scaled_df_no_outliers, centers=k)$tot.withinss})
#plot(k, WSS, type="l", xlab= "Number of k clusters", ylab="Within-clusters sum of squares")
fviz_nbclust(scaled_df_no_outliers, kmeans, method = 'wss')

# Gap statistics method
fviz_nbclust(scaled_df_no_outliers, kmeans, method = 'gap_stat')

# Silhouette method method
fviz_nbclust(scaled_df_no_outliers, kmeans, method = 'silhouette')

## Part C

# Perform k-means clustering with the most favored k from automated tools
k = 2
print(k)
kmeans_result <-
  kmeans(scaled_df_no_outliers, centers = k, nstart = 10)
kmeans_result

# Visualize
fviz_cluster(kmeans_result, data = scaled_df_no_outliers)
fviz_cluster(
  kmeans_result,
  data = scaled_df_no_outliers,
  ellipse.type = "euclid",
  star.plot = TRUE,
  repel = TRUE,
  ggtheme = theme_minimal()
)

# Calculate the between_cluster_sums_of_squares (BSS) and within_cluster_sums_of_squares (WSS) indices
WSS = kmeans_result$tot.withinss
BSS = kmeans_result$betweenss
cat("WSS:", WSS)
cat("BSS:", BSS)

# Calculate ratio of BSS over TSS
TSS <- BSS + WSS
cat("TSS:", TSS)

# Calculate the BSS/TSS Ratio
BSS_ratio <- BSS / TSS
cat("BSS/TSS Ratio:", BSS_ratio)

## Part D

# Silhouette plot
sil <-
  silhouette(kmeans_result$cluster, dist(scaled_df_no_outliers))
fviz_silhouette(sil)

## Part E

# Perform PCA on the scaled dataset
pca <- prcomp(scaled_df_no_outliers)

# Extract eigenvalues and eigenvectors
eigenvalues <- pca$sdev ^ 2
eigenvectors <- pca$rotation

# Print eigenvalues and eigenvectors
cat("Eigenvalues:\n")
print(eigenvalues)
cat("\nEigenvectors:\n")
print(eigenvectors)

# Calculate cumulative score per principal component
cumulative_score <- cumsum(pca$sdev ^ 2 / sum(pca$sdev ^ 2) * 100)

# Print cumulative score per principal component
cat("\nCumulative Score per Principal Component:\n")
print(cumulative_score)

# Choose principal components with cumulative score > 92%
chosen_pcs <- which(cumulative_score > 92)
cat("\nChosen Principal Components:\n")
print(chosen_pcs)

# Extract the principal components from the PCA result
df_pcs <-
  data.frame(pca$x)  # Create a data frame with the principal components
df_pcs

# Print the transformed dataset
cat("\nTransformed Dataset with Principal Components:\n")
print(df_pcs)

## Part F
# Automated tools for determining number of cluster centers
# NBclust methods
set.seed(42)
nbclust_result_pca <-
  NbClust(
    df_pcs,
    distance = "euclidean",
    min.nc = 2,
    max.nc = 10,
    method = "kmeans",
    index = "all"
  )

# Elbow method
#k = 2:10
#set.seed(42)
#WSS = sapply(k, function(k) {kmeans(scaled_df_no_outliers, centers=k)$tot.withinss})
#plot(k, WSS, type="l", xlab= "Number of k clusters", ylab="Within-clusters sum of squares")
fviz_nbclust(df_pcs, kmeans, method = 'wss')

# Gap statistics method
fviz_nbclust(df_pcs, kmeans, method = 'gap_stat')

# Silhouette method method
fviz_nbclust(df_pcs, kmeans, method = 'silhouette')

## Part G

# Perform k-means clustering with the most favored k from automated tools
k = 3
print(k)
kmeans_result_pca <- kmeans(df_pcs, centers = k, nstart = 10)
kmeans_result_pca

# Visualize
fviz_cluster(kmeans_result_pca, data = df_pcs)
fviz_cluster(
  kmeans_result_pca,
  data = df_pcs,
  ellipse.type = "euclid",
  star.plot = TRUE,
  repel = TRUE,
  ggtheme = theme_minimal()
)

# Calculate the between_cluster_sums_of_squares (BSS) and within_cluster_sums_of_squares (WSS) indices
WSS_pca = kmeans_result_pca$tot.withinss
BSS_pca = kmeans_result_pca$betweenss
cat("WSS:", WSS_pca)
cat("BSS:", BSS_pca)

# Calculate ratio of BSS over TSS
TSS_pca <- BSS_pca + WSS_pca
cat("TSS:", TSS)

# Calculate the BSS/TSS Ratio
BSS_ratio_pca <- BSS / TSS
cat("BSS/TSS Ratio:", BSS_ratio_pca)

## Part H

# Silhouette plot
sil <- silhouette(kmeans_result_pca$cluster, dist(df_pcs))
fviz_silhouette(sil)

## Part I
# Calculate cluster membership for each observation
cluster_membership <- kmeans_result_pca$cluster

# Calculate dissimilarity matrix from the transformed dataset
dist_matrix <- dist(df_pcs)

# Calculate Calinski-Harabasz Index
calinski_harabasz <-
  cluster.stats(dist_matrix, cluster_membership)$ch  # Calculate Calinski-Harabasz Index

# Print Calinski-Harabasz Index
cat("Calinski-Harabasz Index:", calinski_harabasz, "\n")

# Plot the clusters using the first two principal components
df_plot <-
  cbind(df_pcs, cluster = as.factor(cluster_membership))  # Combine transformed dataset with cluster membership
ggplot(df_plot, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(
    title = "K-means Clustering with PCA",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  )
