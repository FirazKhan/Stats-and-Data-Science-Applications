#!/usr/bin/env Rscript

# =============================================================================
# Clean Analysis Runner
# =============================================================================

# Clear workspace
rm(list = ls())

# Load required libraries
library(targets)
library(here)
library(ggplot2)
library(dplyr)

# Set working directory
setwd(here())

cat("=== MTHM503 DATA SCIENCE ANALYSIS ===\n")
cat("Running all tasks...\n\n")

# Source all functions
cat("Loading functions...\n")
source("R/functions.R")
source("R/load_data.R")
source("R/utils.R")
source("R/task1_classification.R")
source("R/task2_regression.R")
source("R/task3_unsupervised.R")

# Test database connection
cat("Testing database connection...\n")
con <- get_db_connection()
if (!is.null(con)) {
  cat("✓ Database connected successfully\n")
  DBI::dbDisconnect(con)
} else {
  cat("✗ Database connection failed\n")
  stop("Cannot connect to database")
}

# Load targets pipeline
cat("Loading pipeline...\n")
source("_targets.R")

# Run all analysis
cat("Running analysis...\n")
tar_make()

# Load results
cat("Loading results...\n")
tar_load(classification_results)
tar_load(regression_results)
tar_load(unsupervised_results)
tar_load(classification_plots)
tar_load(regression_plots)
tar_load(unsupervised_plots)

# Load additional model objects needed for plots
cat("Loading model objects...\n")
tar_load(rf_reduced_model)
tar_load(dt_model)
tar_load(rate_model)
tar_load(pedestrian_split)
tar_load(fire_rescue_processed)

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("ANALYSIS RESULTS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Task 1: Classification Results
cat("\n--- TASK 1: CLASSIFICATION ---\n")
cat("Random Forest Accuracy:", round(classification_results$rf_accuracy, 4), "\n")
cat("Decision Tree Accuracy:", round(classification_results$dt_accuracy, 4), "\n")
cat("Overall AUC:", round(classification_results$overall_auc, 4), "\n")

# Task 2: Regression Results
cat("\n--- TASK 2: REGRESSION ---\n")
cat("Best Model:", regression_results$best_model, "\n")
cat("AIC Comparison:\n")
print(regression_results$aic_comparison)

# Task 3: Unsupervised Learning Results
cat("\n--- TASK 3: UNSUPERVISED LEARNING ---\n")
cat("Optimal Components:", unsupervised_results$optimal_components, "\n")
cat("Optimal Clusters:", unsupervised_results$optimal_k, "\n")
cat("K-means Silhouette:", round(unsupervised_results$silhouette_kmeans, 4), "\n")
cat("Hierarchical Silhouette:", round(unsupervised_results$silhouette_hclust, 4), "\n")

# =============================================================================
# DISPLAY PLOTS
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("GENERATING PLOTS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Create plots directory
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Task 1: Classification Plots
tryCatch({
  cat("\n--- TASK 1: CLASSIFICATION PLOTS ---\n")
  tar_load(classification_plots)
  tar_load(classification_results)
  
  cat("Displaying target distribution plot...\n")
  print(classification_plots$target_dist)
  ggsave("plots/task1_target_distribution.png", classification_plots$target_dist, width = 10, height = 6)
  
  cat("Displaying feature importance plot...\n")
  print(classification_plots$feature_importance)
  ggsave("plots/task1_feature_importance.png", classification_plots$feature_importance, width = 10, height = 6)
  
  # Confusion Matrix Heatmap (Publication Quality)
  if (!is.null(classification_results$rf_confusion_matrix)) {
    library(ggplot2)
    library(reshape2)
    
    # Ensure the confusion matrix is a matrix (not a data frame)
    cm <- as.matrix(classification_results$rf_confusion_matrix)
    
    # Melt the matrix for ggplot
    cm_melted <- as.data.frame(as.table(cm))
    colnames(cm_melted) <- c("Predicted", "Actual", "Count")
    
    # Convert to factors for correct ordering (reverse y for heatmap style)
    cm_melted$Predicted <- factor(cm_melted$Predicted, levels = rev(rownames(cm)))
    cm_melted$Actual <- factor(cm_melted$Actual, levels = colnames(cm))
    cm_melted$Count <- as.numeric(cm_melted$Count)
    
    confusion_plot <- ggplot(cm_melted, aes(x = Actual, y = Predicted, fill = Count)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = Count), color = "white", size = 6, fontface = "bold") +
      scale_fill_gradient(low = "#b3d1e6", high = "#001f5c") +
      labs(
        title = "Confusion Matrix - Random Forest Model",
        x = "Actual",
        y = "Predicted"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14)
      )
    
    print(confusion_plot)
    ggsave("plots/task1_confusion_matrix.png", confusion_plot, width = 7, height = 5)
    cat("✓ Confusion matrix plot saved\n")
  }
  
  cat("✓ Classification plots saved to plots/ directory\n")
  
}, error = function(e) {
  cat("Classification plots not available\n")
})

# Task 2: Regression Plots
cat("\n--- TASK 2 PLOTS ---\n")
if (!is.null(regression_plots$interaction)) {
  print(regression_plots$interaction)
  ggsave("plots/task2_regression_interaction.png", regression_plots$interaction, width = 10, height = 6)
  cat("✓ Regression interaction plot saved\n")
}

if (!is.null(regression_plots$interaction_count)) {
  print(regression_plots$interaction_count)
  ggsave("plots/task2_regression_count.png", regression_plots$interaction_count, width = 10, height = 6)
  cat("✓ Regression count plot saved\n")
}

# Task 3: Unsupervised Learning Plots
cat("\n--- TASK 3 PLOTS ---\n")
if (!is.null(unsupervised_plots$scree_plot)) {
  print(unsupervised_plots$scree_plot)
  ggsave("plots/task3_scree_plot.png", unsupervised_plots$scree_plot, width = 10, height = 6)
  cat("✓ Scree plot saved\n")
}

if (!is.null(unsupervised_plots$pca_clustering)) {
  print(unsupervised_plots$pca_clustering)
  ggsave("plots/task3_pca_clustering.png", unsupervised_plots$pca_clustering, width = 10, height = 6)
  cat("✓ PCA clustering plot saved\n")
}

if (!is.null(unsupervised_plots$method_comparison)) {
  print(unsupervised_plots$method_comparison)
  ggsave("plots/task3_method_comparison.png", unsupervised_plots$method_comparison, width = 10, height = 6)
  cat("✓ Method comparison plot saved\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("ANALYSIS COMPLETE\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("✓ All tasks completed successfully\n")
cat("✓ Results displayed above\n")
cat("✓ Plots saved to plots/ directory\n")
cat("✓ Analysis ready for report generation\n")
