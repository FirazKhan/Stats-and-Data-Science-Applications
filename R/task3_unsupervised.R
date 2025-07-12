# R/task3_unsupervised.R
# Task 3: Unsupervised Learning Functions

library(RPostgres)
library(DBI)
library(ggplot2)
library(dplyr)
library(cluster)

#' Load olive oil data
#' @return Data frame with olive oil composition data
load_olive_oil_data <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PGRDATABASE"),
    host = Sys.getenv("PGRHOST"),
    user = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    port = Sys.getenv("PGRPORT")
  )
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, "SELECT * FROM olive_oil")
}

#' Preprocess olive oil data
#' @param data Raw olive oil data
#' @return Preprocessed and scaled data
preprocess_olive_oil_data <- function(data) {
  # Get numeric columns
  fatty_acid_cols <- names(data)[sapply(data, is.numeric)]
  
  if (length(fatty_acid_cols) == 0) {
    stop("No numeric columns available for analysis")
  }
  
  # Extract fatty acid data
  olive_oil_fatty_acids <- data[, fatty_acid_cols, drop = FALSE]
  
  # Remove rows with missing values
  olive_oil_fatty_acids <- olive_oil_fatty_acids[complete.cases(olive_oil_fatty_acids), ]
  
  # Remove zero variance variables
  variances <- apply(olive_oil_fatty_acids, 2, var)
  zero_var_cols <- names(variances[variances == 0])
  
  if (length(zero_var_cols) > 0) {
    olive_oil_fatty_acids <- olive_oil_fatty_acids[, variances > 0, drop = FALSE]
  }
  
  # Apply standardization
  olive_oil_scaled <- scale(olive_oil_fatty_acids)
  
  list(
    raw_data = olive_oil_fatty_acids,
    scaled_data = olive_oil_scaled,
    fatty_acid_cols = names(olive_oil_fatty_acids)
  )
}

#' Perform PCA
#' @param data Processed olive oil data
#' @return PCA results
perform_pca <- function(data) {
  pca_result <- prcomp(data$scaled_data, center = TRUE, scale. = TRUE)
  
  variance_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
  cumulative_var <- cumsum(variance_explained)
  
  optimal_components <- which(cumulative_var >= 80)[1]
  if (is.na(optimal_components)) {
    optimal_components <- length(cumulative_var)
  }
  
  list(
    pca_result = pca_result,
    variance_explained = variance_explained,
    cumulative_var = cumulative_var,
    optimal_components = optimal_components
  )
}

#' Perform K-means clustering
#' @param data Processed olive oil data
#' @return K-means clustering results
perform_kmeans_clustering <- function(data) {
  # Determine optimal number of clusters
  max_k <- min(10, nrow(data$scaled_data) - 1)
  wss <- numeric(max_k)
  
  for (k in 1:max_k) {
    tryCatch({
      kmeans_temp <- kmeans(data$scaled_data, centers = k, nstart = 25)
      wss[k] <- kmeans_temp$tot.withinss
    }, error = function(e) {
      wss[k] <- NA
    })
  }
  
  # Use k=3 as optimal (can be adjusted based on elbow method)
  optimal_k <- 3
  set.seed(123)
  kmeans_result <- kmeans(data$scaled_data, centers = optimal_k, nstart = 25)
  
  list(
    kmeans_result = kmeans_result,
    wss = wss,
    optimal_k = optimal_k
  )
}

#' Perform hierarchical clustering
#' @param data Processed olive oil data
#' @return Hierarchical clustering results
perform_hierarchical_clustering <- function(data) {
  dist_matrix <- dist(data$scaled_data)
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  
  # Use same optimal k as k-means
  optimal_k <- 3
  hclust_clusters <- cutree(hclust_result, k = optimal_k)
  
  list(
    hclust_result = hclust_result,
    hclust_clusters = hclust_clusters,
    optimal_k = optimal_k
  )
}

#' Evaluate unsupervised models
#' @param pca_result PCA results
#' @param kmeans_result K-means results
#' @param hclust_result Hierarchical clustering results
#' @param data Processed data
#' @return List with evaluation results
evaluate_unsupervised_models <- function(pca_result, kmeans_result, hclust_result, data) {
  # Calculate distance matrix for silhouette scores
  dist_matrix <- dist(data$scaled_data)
  
  # K-means evaluation
  silhouette_kmeans <- silhouette(kmeans_result$kmeans_result$cluster, dist_matrix)
  silhouette_avg_kmeans <- mean(silhouette_kmeans[, 3])
  
  # Hierarchical clustering evaluation
  silhouette_hclust <- silhouette(hclust_result$hclust_clusters, dist_matrix)
  silhouette_avg_hclust <- mean(silhouette_hclust[, 3])
  
  # K-means cluster profiles
  olive_oil_with_clusters <- data$raw_data
  olive_oil_with_clusters$kmeans_cluster <- kmeans_result$kmeans_result$cluster
  olive_oil_with_clusters$hclust_cluster <- hclust_result$hclust_clusters
  
  kmeans_profiles <- olive_oil_with_clusters %>%
    group_by(kmeans_cluster) %>%
    summarise(across(all_of(data$fatty_acid_cols), list(mean = mean, sd = sd)))
  
  hclust_profiles <- olive_oil_with_clusters %>%
    group_by(hclust_cluster) %>%
    summarise(across(all_of(data$fatty_acid_cols), list(mean = mean, sd = sd)))
  
  list(
    silhouette_kmeans = silhouette_avg_kmeans,
    silhouette_hclust = silhouette_avg_hclust,
    kmeans_profiles = kmeans_profiles,
    hclust_profiles = hclust_profiles,
    kmeans_clusters = kmeans_result$kmeans_result$cluster,
    hclust_clusters = hclust_result$hclust_clusters
  )
}

#' Create unsupervised learning plots
#' @param results Unsupervised learning results
#' @param pca_result PCA results
#' @param data Processed data
#' @return List of plots
create_unsupervised_plots <- function(results, pca_result, data) {
  plots <- list()
  
  # Scree Plot
  scree_data <- data.frame(
    PC = 1:length(pca_result$variance_explained), 
    Variance = pca_result$variance_explained,
    Cumulative = pca_result$cumulative_var
  )
  
  plots$scree <- ggplot(scree_data, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(aes(y = Cumulative), color = "red", size = 1) +
    geom_point(aes(y = Cumulative), color = "red", size = 3) +
    labs(title = "Scree Plot - Variance Explained by Principal Components",
         x = "Principal Component", y = "Variance Explained (%)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # K-means Clustering Results on PCA
  if (!is.null(pca_result$pca_result) && ncol(pca_result$pca_result$x) >= 2) {
    plot_data <- data.frame(
      PC1 = pca_result$pca_result$x[, 1], 
      PC2 = pca_result$pca_result$x[, 2],
      Cluster = factor(results$kmeans_clusters)
    )
    
    plots$kmeans_pca <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.8, size = 3) +
      scale_color_brewer(palette = "Set1") +
      labs(title = "K-means Clustering Results (PCA Space)",
           x = paste("PC1 (", round(pca_result$variance_explained[1], 1), "%)"),
           y = paste("PC2 (", round(pca_result$variance_explained[2], 1), "%)"),
           color = "Cluster") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            legend.position = "bottom",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12))
  }
  
  plots
} 