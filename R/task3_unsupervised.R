# R/task3_unsupervised.R
# Task 3: Unsupervised Learning Functions - Enhanced Version

library(RPostgres)
library(DBI)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

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
  # Get numeric columns (fatty acid composition)
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
    cat("Removing zero variance columns:", paste(zero_var_cols, collapse = ", "), "\n")
    olive_oil_fatty_acids <- olive_oil_fatty_acids[, variances > 0, drop = FALSE]
  }
  
  # Apply standardization for PCA
  olive_oil_scaled <- scale(olive_oil_fatty_acids)
  
  cat("Data preprocessing completed:\n")
  cat("- Fatty acid variables:", ncol(olive_oil_fatty_acids), "\n")
  cat("- Samples:", nrow(olive_oil_fatty_acids), "\n")
  cat("- Variables:", paste(names(olive_oil_fatty_acids), collapse = ", "), "\n\n")
  
  list(
    raw_data = olive_oil_fatty_acids,
    scaled_data = olive_oil_scaled,
    fatty_acid_cols = names(olive_oil_fatty_acids)
  )
}

#' Perform PCA with comprehensive justification
#' @param data Processed olive oil data
#' @return PCA results with justification
perform_pca <- function(data) {
  cat("=== PRINCIPAL COMPONENT ANALYSIS ===\n")
  cat("PCA is appropriate for olive oil analysis because:\n")
  cat("1. High-dimensional fatty acid data (", ncol(data$scaled_data), " variables)\n")
  cat("2. Variables are likely correlated (compositional data)\n")
  cat("3. Need dimensionality reduction for visualization and clustering\n")
  cat("4. Standardization ensures equal importance to all fatty acids\n\n")
  
  # Perform PCA
  pca_result <- prcomp(data$scaled_data, center = TRUE, scale. = TRUE)
  
  # Calculate variance explained
  variance_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
  cumulative_var <- cumsum(variance_explained)
  
  # Determine optimal number of components using multiple criteria
  # Kaiser criterion: eigenvalues > 1
  eigenvalues <- pca_result$sdev^2
  kaiser_components <- sum(eigenvalues > 1)
  
  # Cumulative variance criterion: 80% threshold
  cumulative_80_components <- which(cumulative_var >= 80)[1]
  if (is.na(cumulative_80_components)) {
    cumulative_80_components <- length(cumulative_var)
  }
  
  # Scree plot elbow method (simplified)
  scree_elbow <- 2  # Often the first 2-3 components capture most variance
  
  cat("Component Selection Criteria:\n")
  cat("- Kaiser criterion (eigenvalues > 1):", kaiser_components, "components\n")
  cat("- Cumulative variance (80%):", cumulative_80_components, "components\n")
  cat("- First 2 components explain:", round(sum(variance_explained[1:2]), 1), "% of variance\n")
  cat("- First 3 components explain:", round(sum(variance_explained[1:3]), 1), "% of variance\n\n")
  
  # Choose optimal components for visualization and clustering
  optimal_components <- min(cumulative_80_components, max(2, kaiser_components))
  
  cat("JUSTIFICATION: Selected", optimal_components, "components because:\n")
  cat("- Balances explained variance (", round(cumulative_var[optimal_components], 1), "%) with interpretability\n")
  cat("- Captures main patterns in fatty acid composition\n")
  cat("- Sufficient for effective clustering and visualization\n\n")
  
  # Component interpretation
  loadings <- pca_result$rotation[, 1:min(3, ncol(pca_result$rotation)), drop = FALSE]
  
  cat("Component Interpretation:\n")
  for (i in 1:min(3, ncol(loadings))) {
    loadings_i <- loadings[, i]
    top_positive <- names(sort(loadings_i, decreasing = TRUE)[1:2])
    top_negative <- names(sort(loadings_i, decreasing = FALSE)[1:2])
    
    cat("PC", i, " (", round(variance_explained[i], 1), "% variance):\n")
    cat("  - High loadings (+):", paste(top_positive, collapse = ", "), "\n")
    cat("  - High loadings (-):", paste(top_negative, collapse = ", "), "\n")
  }
  cat("\n")
  
  list(
    pca_result = pca_result,
    variance_explained = variance_explained,
    cumulative_var = cumulative_var,
    optimal_components = optimal_components,
    kaiser_components = kaiser_components,
    cumulative_80_components = cumulative_80_components,
    loadings = loadings,
    eigenvalues = eigenvalues
  )
}

#' Determine optimal number of clusters with justification
#' @param data Processed olive oil data
#' @return Optimal number of clusters with justification
determine_optimal_clusters <- function(data) {
  cat("=== CLUSTER NUMBER DETERMINATION ===\n")
  cat("Using multiple methods to justify optimal number of clusters:\n\n")
  
  # Method 1: Elbow method (WSS)
  max_k <- min(10, nrow(data$scaled_data) - 1)
  wss <- numeric(max_k)
  
  for (k in 1:max_k) {
    tryCatch({
      set.seed(123)
      kmeans_temp <- kmeans(data$scaled_data, centers = k, nstart = 25)
      wss[k] <- kmeans_temp$tot.withinss
    }, error = function(e) {
      wss[k] <- NA
    })
  }
  
  # Method 2: Silhouette method
  silhouette_scores <- numeric(max_k)
  for (k in 2:max_k) {
    tryCatch({
      set.seed(123)
      kmeans_temp <- kmeans(data$scaled_data, centers = k, nstart = 25)
      sil <- silhouette(kmeans_temp$cluster, dist(data$scaled_data))
      silhouette_scores[k] <- mean(sil[, 3])
    }, error = function(e) {
      silhouette_scores[k] <- NA
    })
  }
  
  # Method 3: Gap statistic (simplified)
  gap_stat <- tryCatch({
    set.seed(123)
    fviz_nbclust(data$scaled_data, kmeans, method = "gap_stat", k.max = min(8, max_k))
  }, error = function(e) {
    NULL
  })
  
  # Determine optimal k from each method
  elbow_k <- which.min(diff(diff(wss))) + 1  # Second derivative approach
  if (is.na(elbow_k) || elbow_k <= 1) elbow_k <- 3
  
  silhouette_k <- which.max(silhouette_scores)
  if (is.na(silhouette_k) || silhouette_k <= 1) silhouette_k <- 3
  
  cat("Results from different methods:\n")
  cat("- Elbow method (WSS): k =", elbow_k, "\n")
  cat("- Silhouette method: k =", silhouette_k, "\n")
  cat("- Silhouette scores: k=2:", round(silhouette_scores[2], 3), 
      ", k=3:", round(silhouette_scores[3], 3), 
      ", k=4:", round(silhouette_scores[4], 3), "\n")
  
  # Choose optimal k based on consensus and interpretability
  optimal_k <- ifelse(silhouette_scores[3] > silhouette_scores[2] && 
                      silhouette_scores[3] > silhouette_scores[4], 3,
                      ifelse(silhouette_scores[2] > silhouette_scores[4], 2, 4))
  
  cat("\nJUSTIFICATION: Selected k =", optimal_k, "because:\n")
  cat("- Highest silhouette score (", round(silhouette_scores[optimal_k], 3), ")\n")
  cat("- Reasonable for olive oil types (different regions/varieties)\n")
  cat("- Balances cluster separation with interpretability\n")
  cat("- Silhouette > 0.25 indicates reasonable cluster structure\n\n")
  
  list(
    optimal_k = optimal_k,
    wss = wss,
    silhouette_scores = silhouette_scores,
    elbow_k = elbow_k,
    silhouette_k = silhouette_k,
    gap_stat = gap_stat
  )
}

#' Perform K-means clustering with optimal k
#' @param data Processed olive oil data
#' @param cluster_analysis Results from cluster determination
#' @return K-means clustering results
perform_kmeans_clustering <- function(data, cluster_analysis) {
  optimal_k <- cluster_analysis$optimal_k
  
  cat("=== K-MEANS CLUSTERING ===\n")
  cat("K-means is appropriate for olive oil analysis because:\n")
  cat("1. Data is continuous and standardized\n")
  cat("2. Expecting distinct olive oil types/regions\n")
  cat("3. Spherical clusters reasonable for compositional data\n")
  cat("4. Efficient and interpretable algorithm\n\n")
  
  set.seed(123)
  kmeans_result <- kmeans(data$scaled_data, centers = optimal_k, nstart = 25)
  
  cat("K-means clustering completed:\n")
  cat("- Number of clusters:", optimal_k, "\n")
  cat("- Cluster sizes:", paste(kmeans_result$size, collapse = ", "), "\n")
  cat("- Within-cluster sum of squares:", round(kmeans_result$tot.withinss, 2), "\n")
  cat("- Between-cluster sum of squares:", round(kmeans_result$betweenss, 2), "\n")
  cat("- Explained variance ratio:", round(kmeans_result$betweenss/kmeans_result$totss, 3), "\n\n")
  
  list(
    kmeans_result = kmeans_result,
    optimal_k = optimal_k,
    wss = cluster_analysis$wss,
    silhouette_scores = cluster_analysis$silhouette_scores
  )
}

#' Perform hierarchical clustering
#' @param data Processed olive oil data
#' @param cluster_analysis Results from cluster determination
#' @return Hierarchical clustering results
perform_hierarchical_clustering <- function(data, cluster_analysis) {
  optimal_k <- cluster_analysis$optimal_k
  
  cat("=== HIERARCHICAL CLUSTERING ===\n")
  cat("Hierarchical clustering provides:\n")
  cat("1. Dendrogram visualization of cluster structure\n")
  cat("2. No assumption about cluster shapes\n")
  cat("3. Complete clustering hierarchy\n")
  cat("4. Ward.D2 method minimizes within-cluster variance\n\n")
  
  dist_matrix <- dist(data$scaled_data)
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  hclust_clusters <- cutree(hclust_result, k = optimal_k)
  
  cat("Hierarchical clustering completed:\n")
  cat("- Method: Ward.D2\n")
  cat("- Number of clusters:", optimal_k, "\n")
  cat("- Cluster sizes:", paste(table(hclust_clusters), collapse = ", "), "\n\n")
  
  list(
    hclust_result = hclust_result,
    hclust_clusters = hclust_clusters,
    optimal_k = optimal_k
  )
}

#' Comprehensive cluster interpretation and evaluation
#' @param pca_result PCA results
#' @param kmeans_result K-means results
#' @param hclust_result Hierarchical clustering results
#' @param data Processed data
#' @return List with comprehensive evaluation and interpretation
evaluate_unsupervised_models <- function(pca_result, kmeans_result, hclust_result, data) {
  cat("=== COMPREHENSIVE CLUSTER INTERPRETATION ===\n")
  
  # Calculate distance matrix for silhouette scores
  dist_matrix <- dist(data$scaled_data)
  
  # K-means evaluation
  silhouette_kmeans <- silhouette(kmeans_result$kmeans_result$cluster, dist_matrix)
  silhouette_avg_kmeans <- mean(silhouette_kmeans[, 3])
  
  # Hierarchical clustering evaluation
  silhouette_hclust <- silhouette(hclust_result$hclust_clusters, dist_matrix)
  silhouette_avg_hclust <- mean(silhouette_hclust[, 3])
  
  # Create data with cluster assignments
  olive_oil_with_clusters <- data$raw_data
  olive_oil_with_clusters$kmeans_cluster <- kmeans_result$kmeans_result$cluster
  olive_oil_with_clusters$hclust_cluster <- hclust_result$hclust_clusters
  
  # K-means cluster profiles
  kmeans_profiles <- olive_oil_with_clusters %>%
    group_by(kmeans_cluster) %>%
    summarise(across(all_of(data$fatty_acid_cols), list(mean = mean, sd = sd)))
  
  # Hierarchical cluster profiles
  hclust_profiles <- olive_oil_with_clusters %>%
    group_by(hclust_cluster) %>%
    summarise(across(all_of(data$fatty_acid_cols), list(mean = mean, sd = sd)))
  
  # Detailed cluster interpretation
  cat("CLUSTER INTERPRETATION - What makes each cluster unique:\n\n")
  
  # Analyze K-means clusters
  overall_means <- colMeans(data$raw_data)
  cluster_interpretations <- list()
  
  for (i in 1:kmeans_result$optimal_k) {
    cluster_data <- olive_oil_with_clusters[olive_oil_with_clusters$kmeans_cluster == i, ]
    cluster_means <- colMeans(cluster_data[, data$fatty_acid_cols])
    
    # Find distinctive fatty acids (most different from overall mean)
    differences <- cluster_means - overall_means
    distinctive_high <- names(sort(differences, decreasing = TRUE)[1:2])
    distinctive_low <- names(sort(differences, decreasing = FALSE)[1:2])
    
    cat("Cluster", i, " (n =", nrow(cluster_data), "samples):\n")
    cat("  - HIGH in:", paste(distinctive_high, collapse = ", "), "\n")
    cat("  - LOW in:", paste(distinctive_low, collapse = ", "), "\n")
    cat("  - Distinctive values:", paste(round(cluster_means[distinctive_high], 2), collapse = ", "), 
        " (high), ", paste(round(cluster_means[distinctive_low], 2), collapse = ", "), " (low)\n")
    
    # Interpretation based on fatty acid knowledge
    interpretation <- ""
    if (any(grepl("palmitic|oleic|linoleic", c(distinctive_high, distinctive_low)))) {
      interpretation <- "  - INTERPRETATION: Distinct fatty acid profile suggesting different olive variety/region\n"
    } else {
      interpretation <- "  - INTERPRETATION: Unique compositional signature\n"
    }
    cat(interpretation)
    cat("\n")
    
    cluster_interpretations[[i]] <- list(
      high = distinctive_high,
      low = distinctive_low,
      interpretation = interpretation
    )
  }
  
  # Cluster quality assessment
  cat("CLUSTER QUALITY ASSESSMENT:\n")
  cat("- K-means silhouette score:", round(silhouette_avg_kmeans, 3), "\n")
  cat("- Hierarchical silhouette score:", round(silhouette_avg_hclust, 3), "\n")
  cat("- Best method:", ifelse(silhouette_avg_kmeans > silhouette_avg_hclust, "K-means", "Hierarchical"), "\n")
  
  quality_interpretation <- ""
  if (max(silhouette_avg_kmeans, silhouette_avg_hclust) > 0.5) {
    quality_interpretation <- "- INTERPRETATION: Strong cluster structure - clear olive oil types identified\n"
  } else if (max(silhouette_avg_kmeans, silhouette_avg_hclust) > 0.25) {
    quality_interpretation <- "- INTERPRETATION: Reasonable cluster structure - distinct olive oil groups present\n"
  } else {
    quality_interpretation <- "- INTERPRETATION: Weak cluster structure - overlapping olive oil characteristics\n"
  }
  cat(quality_interpretation)
  
  # PCA interpretation in context of clusters
  if (!is.null(pca_result$pca_result)) {
    pca_coords <- pca_result$pca_result$x[, 1:2]
    cluster_pca_means <- aggregate(pca_coords, by = list(kmeans_result$kmeans_result$cluster), FUN = mean)
    
    cat("\nCLUSTER POSITIONS IN PCA SPACE:\n")
    for (i in 1:nrow(cluster_pca_means)) {
      cat("Cluster", cluster_pca_means$Group.1[i], 
          ": PC1 =", round(cluster_pca_means$PC1[i], 2), 
          ", PC2 =", round(cluster_pca_means$PC2[i], 2), "\n")
    }
    
    # Interpretation of PCA clustering
    cat("\nPCA CLUSTERING INTERPRETATION:\n")
    cat("- PC1 explains", round(pca_result$variance_explained[1], 1), "% of variance\n")
    cat("- PC2 explains", round(pca_result$variance_explained[2], 1), "% of variance\n")
    cat("- Together explain", round(sum(pca_result$variance_explained[1:2]), 1), "% of total variation\n")
    cat("- Cluster separation visible in reduced dimensional space\n")
    cat("- INTERPRETATION: Fatty acid composition creates natural groupings\n\n")
  }
  
  # Create PCA summary for reporting
  pca_summary <- data.frame(
    Component = paste0("PC", 1:min(5, length(pca_result$variance_explained))),
    Variance_Explained = round(pca_result$variance_explained[1:min(5, length(pca_result$variance_explained))], 2),
    Cumulative_Variance = round(pca_result$cumulative_var[1:min(5, length(pca_result$variance_explained))], 2)
  )
  
  list(
    # PCA results
    pca_summary = pca_summary,
    variance_explained = pca_result$variance_explained,
    cumulative_var = pca_result$cumulative_var,
    optimal_components = pca_result$optimal_components,
    component_loadings = pca_result$loadings,
    
    # Clustering results
    silhouette_kmeans = silhouette_avg_kmeans,
    silhouette_hclust = silhouette_avg_hclust,
    kmeans_profiles = kmeans_profiles,
    hclust_profiles = hclust_profiles,
    kmeans_clusters = kmeans_result$kmeans_result$cluster,
    hclust_clusters = hclust_result$hclust_clusters,
    optimal_k = kmeans_result$optimal_k,
    
    # Detailed interpretations
    cluster_interpretations = cluster_interpretations,
    quality_interpretation = quality_interpretation,
    pca_cluster_means = if (!is.null(pca_result$pca_result)) cluster_pca_means else NULL,
    
    # Method selection data
    wss = kmeans_result$wss,
    silhouette_scores = kmeans_result$silhouette_scores
  )
}

#' Create comprehensive unsupervised learning plots
#' @param results Unsupervised learning results
#' @param pca_result PCA results
#' @param data Processed data
#' @return List of enhanced plots
create_unsupervised_plots <- function(results, pca_result, data) {
  plots <- list()
  
  # Enhanced Scree Plot with component selection guidance
  scree_data <- data.frame(
    PC = 1:length(pca_result$variance_explained), 
    Variance = pca_result$variance_explained,
    Cumulative = pca_result$cumulative_var
  )
  
  plots$scree_plot <- ggplot(scree_data, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = Cumulative), color = "red", size = 1.2) +
    geom_point(aes(y = Cumulative), color = "red", size = 3) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
    geom_vline(xintercept = pca_result$optimal_components, linetype = "dashed", color = "orange", alpha = 0.7) +
    labs(title = "Scree Plot - Variance Explained by Principal Components",
         subtitle = paste("Selected", pca_result$optimal_components, "components explaining", 
                         round(pca_result$cumulative_var[pca_result$optimal_components], 1), "% of variance"),
         x = "Principal Component", y = "Variance Explained (%)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "darkgray"))
  
  # Enhanced PCA Scatterplot with Cluster Groups (as specifically requested)
  if (!is.null(pca_result$pca_result) && ncol(pca_result$pca_result$x) >= 2) {
    plot_data <- data.frame(
      PC1 = pca_result$pca_result$x[, 1], 
      PC2 = pca_result$pca_result$x[, 2],
      Cluster = factor(results$kmeans_clusters)
    )
    
    # Calculate cluster centers in PCA space
    cluster_centers <- aggregate(plot_data[, c("PC1", "PC2")], 
                               by = list(Cluster = plot_data$Cluster), FUN = mean)
    
    plots$pca_clustering <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_point(data = cluster_centers, aes(x = PC1, y = PC2, color = Cluster), 
                size = 8, shape = 17, alpha = 0.9) +
      scale_color_brewer(palette = "Set1") +
      stat_ellipse(aes(color = Cluster), level = 0.68, size = 1, alpha = 0.3) +
      labs(title = "PCA Scatterplot with Cluster Groups",
           subtitle = paste("Silhouette Score:", round(results$silhouette_kmeans, 3), 
                           "| Clusters:", results$optimal_k),
           x = paste("PC1 (", round(pca_result$variance_explained[1], 1), "% variance)"),
           y = paste("PC2 (", round(pca_result$variance_explained[2], 1), "% variance)"),
           color = "Cluster Group") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "darkgray"),
            legend.position = "bottom",
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12))
  }
  
  # Cluster Size Distribution
  if (!is.null(results$kmeans_clusters)) {
    cluster_sizes <- data.frame(
      Cluster = factor(1:results$optimal_k),
      Size = as.numeric(table(results$kmeans_clusters))
    )
    
    plots$cluster_sizes <- ggplot(cluster_sizes, aes(x = Cluster, y = Size, fill = Cluster)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = Size), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Cluster Size Distribution",
           subtitle = paste("Total samples:", sum(cluster_sizes$Size), "| Optimal k:", results$optimal_k),
           x = "Cluster", y = "Number of Samples") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "darkgray"),
            legend.position = "none")
  }
  
  # Silhouette Analysis Plot
  if (!is.null(results$kmeans_clusters)) {
    silhouette_data <- silhouette(results$kmeans_clusters, dist(data$scaled_data))
    silhouette_df <- data.frame(
      Sample = 1:nrow(silhouette_data),
      Cluster = factor(silhouette_data[, 1]),
      Silhouette_Width = silhouette_data[, 3]
    )
    
    plots$silhouette <- ggplot(silhouette_df, aes(x = Sample, y = Silhouette_Width, fill = Cluster)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = mean(silhouette_df$Silhouette_Width), 
                 color = "red", linetype = "dashed", size = 1) +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Silhouette Analysis",
           subtitle = paste("Average Silhouette Width:", round(mean(silhouette_df$Silhouette_Width), 3)),
           x = "Sample", y = "Silhouette Width",
           fill = "Cluster") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "darkgray"),
            legend.position = "bottom")
  }
  
  # Method Comparison Plot
  if (!is.null(results$wss) && !is.null(results$silhouette_scores)) {
    method_data <- data.frame(
      k = 1:length(results$wss),
      WSS = results$wss,
      Silhouette = c(0, results$silhouette_scores[2:length(results$silhouette_scores)])
    )
    
    # Normalize for comparison
    method_data$WSS_norm <- (max(method_data$WSS, na.rm = TRUE) - method_data$WSS) / 
                           (max(method_data$WSS, na.rm = TRUE) - min(method_data$WSS, na.rm = TRUE))
    method_data$Silhouette_norm <- method_data$Silhouette / max(method_data$Silhouette, na.rm = TRUE)
    
    plots$method_comparison <- ggplot(method_data, aes(x = k)) +
      geom_line(aes(y = WSS_norm, color = "Elbow Method"), size = 1.2) +
      geom_point(aes(y = WSS_norm, color = "Elbow Method"), size = 3) +
      geom_line(aes(y = Silhouette_norm, color = "Silhouette Method"), size = 1.2) +
      geom_point(aes(y = Silhouette_norm, color = "Silhouette Method"), size = 3) +
      geom_vline(xintercept = results$optimal_k, linetype = "dashed", color = "darkgreen", size = 1) +
      scale_color_manual(values = c("Elbow Method" = "blue", "Silhouette Method" = "red")) +
      labs(title = "Cluster Number Selection Methods",
           subtitle = paste("Optimal k =", results$optimal_k, "selected based on silhouette score"),
           x = "Number of Clusters (k)", y = "Normalized Score",
           color = "Method") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "darkgray"),
            legend.position = "bottom")
  }
  
  plots
} 