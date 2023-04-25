library(readxl) # for reading Excel files
library(dplyr) # for data manipulation and data wrangling tasks
library(outliers) # for detecting and handling outliers in df
library(cluster) #for clustering
library(NbClust) #for determining optimal number of clusters
library(factoextra) # for visualizing multivariate data analysis results
library(fpc)  # used for calculating Calinski-Harabasz Index
library(ggplot2)  # for data visualization

# Loading the dataset
dfo <- read_excel("./datasets/vehicles.xlsx")
dfo
dim(dfo)
str(dfo)
summary(dfo)

## PART A : Pre-processing

# Removing missing values
df_nomissing <- na.omit(dfo)
dim(dfo)

# Select only the 18 attributes for outlier removal and clustering
df18 <- df_nomissing[, 2:19]
df18
dim(df18)
summary(df18)

# Outlier removal
df_outlier_removal <- df18 # assigning the dataframe to a new dataframe for outlier removal

boxplot(df_outlier_removal) # boxplot after scaling with zscore normalization

# Plot boxplots for each column of dataset
for (col in colnames(df_outlier_removal)) {
  # Create box plot for the current attribute
  boxplot(df_outlier_removal[[col]], main = paste("Boxplot of", col), ylab = col, col = "lightgreen")
}
# Rad.Ra, Pr.Axis.Ra, Max.L.Ra, Sc.Var.Maxis, Sc.Var.maxis, Skew.Maxis, Skew.maxis, Kurt.maxis has outliers

for (col in colnames(df_outlier_removal)) {
  # Calculate IQR for the current column
  Q1 <- quantile(df_outlier_removal[[col]], 0.25)
  Q3 <- quantile(df_outlier_removal[[col]], 0.75)
  IQR <- Q3 - Q1
  # Identify rows with outliers based on IQR threshold
  outlier_threshold <- 1.5 * IQR
  lower_bound <- (Q1 - outlier_threshold)
  upper_bound <- (Q3 + outlier_threshold)
  
  outlier_rows <- df_outlier_removal[[col]] < lower_bound | df_outlier_removal[[col]] > upper_bound
  
  # Print the number of outlier rows in the curret column
  cat("No. of outlier rows in", col, ": ", sum(outlier_rows), "\n")
  
  if (sum(outlier_rows) > 0) {
    # Print row numbers with outlier values for the current column
    cat("  Outlier row nums in ", col, ":", toString(which(outlier_rows)), "\n")
  }
  # Remove rows with outliers from the dataframe
  df_outlier_removal <- df_outlier_removal[!outlier_rows, ]
}


df_no_outliers <- df_outlier_removal # Adding to a new dataframe with descriptive name
df_no_outliers
dim(df_no_outliers)
summary(df_no_outliers)

boxplot(df_no_outliers) # boxplot after outlier removal
# Plot boxplots for each column of dataset (after outlier removal)
for (col in colnames(df_no_outliers)) {
  # Create box plot for the current attribute
  boxplot(df_no_outliers[[col]], main = paste("Boxplot of", col), ylab = col, col = "lightblue")
}

# Normalization
# Scaling data using z-score normalization
scaled_df_no_outliers <- scale(df_no_outliers)
scaled_df_no_outliers <- as.data.frame(scaled_df_no_outliers)
scaled_df_no_outliers
dim(scaled_df_no_outliers)

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

table(nbclust_result$Best.n[1,])


barplot(table(nbclust_result$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria")


# Elbow method
#k = 2:10
set.seed(42)
#WSS = sapply(k, function(k) {kmeans(scaled_df_no_outliers, centers=k)$tot.withinss})
#plot(k, WSS, type="l", xlab= "Number of k clusters", ylab="Within-clusters sum of squares")
fviz_nbclust(scaled_df_no_outliers, kmeans, method = 'wss')

# Gap statistics method
fviz_nbclust(scaled_df_no_outliers, kmeans, method = 'gap_stat')

# Silhouette method method
fviz_nbclust(scaled_df_no_outliers, kmeans, method = 'silhouette')


## Part C

# Perform k-means clustering with the most favored k from automated tools
k = 3
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
TSS = kmeans_result$totss
cat("WSS:", WSS)
cat("BSS:", BSS)
cat("TSS:", TSS)

# Calculate ratio of BSS over TSS
BSS_ratio <- BSS / TSS
cat("BSS/TSS Ratio:", BSS_ratio)


## Part D

# Silhouette plot
sil <- silhouette(kmeans_result$cluster, dist(scaled_df_no_outliers))
fviz_silhouette(sil)



## Part E

# Performing PCA on the scaled dataset
pca <- prcomp(scaled_df_no_outliers)
summary(pca)

# Extracting eigenvalues and eigenvectors
eigenvalues <- pca$sdev ^ 2
eigenvectors <- pca$rotation

# Printing eigenvalues and eigenvectors
cat("Eigenvalues:\n")
print(eigenvalues)
cat("\nEigenvectors:\n")
print(eigenvectors)

# Calculating cumulative score per principal component
cumulative_score <- cumsum(pca$sdev ^ 2 / sum(pca$sdev ^ 2) * 100)

# Printing cumulative score per principal component
cat("\nCumulative Score per Principal Component:\n")
print(cumulative_score)

# Choosin principal components with cumulative score > 92%
chosen_pcs <- which(cumulative_score > 92)
cat("\nChosen Principal Components:\n")
print(chosen_pcs)

# Getting the first two principal components that provide a cumulative proportion of variance above 92%
df_pcs <- as.data.frame(pca$x[, chosen_pcs[1:2]])
# df_pcs <- as.data.frame(pca$x[, chosen_pcs])

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
k = 2
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
TSS_pca <- kmeans_result_pca$totss
cat("WSS:", WSS_pca)
cat("BSS:", BSS_pca)
cat("TSS:", TSS_pca)

# Calculate the BSS/TSS Ratio
BSS_ratio_pca <- BSS_pca / TSS_pca
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


