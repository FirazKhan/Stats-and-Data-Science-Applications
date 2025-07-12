#!/usr/bin/env Rscript

# =============================================================================
# Simple Analysis Runner
# =============================================================================

# Clear workspace
rm(list = ls())

# Load required libraries
library(targets)
library(here)
library(ggplot2)

# Set working directory
setwd(here())

cat("=== Data Science Analysis ===\n")

# Source all functions
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
source("_targets.R")

# Run analysis
cat("Running analysis...\n")
tar_make()

# Load and display results
cat("\n=== TASK RESULTS ===\n")

# Task 1: Classification Results
tryCatch({
  cat("\n--- TASK 1: CLASSIFICATION ---\n")
  tar_load(classification_results)
  cat("=== FEATURE SELECTION RESULTS ===\n")
  cat("Selected Features:", paste(classification_results$selected_features, collapse = ", "), "\n")
  cat("Feature Reduction:", sprintf("%.1f%%", classification_results$feature_reduction_percent), "\n")
  cat("Accuracy Loss:", sprintf("%.2f%%", classification_results$accuracy_loss_percent), "\n\n")
  
  cat("=== MODEL PERFORMANCE COMPARISON ===\n")
  cat("Full Random Forest Accuracy:", round(classification_results$rf_full_accuracy, 4), "\n")
  cat("Parsimonious Random Forest Accuracy:", round(classification_results$rf_reduced_accuracy, 4), "\n")
  cat("Decision Tree Accuracy:", round(classification_results$dt_accuracy, 4), "\n")
  cat("Overall AUC (Full RF):", round(classification_results$overall_auc_full, 4), "\n")
  cat("Overall AUC (Reduced RF):", round(classification_results$overall_auc_reduced, 4), "\n")
  
  cat("\n=== FEATURE IMPORTANCE RANKINGS ===\n")
  cat("Top 10 Most Important Features:\n")
  print(head(classification_results$importance_df, 10))
}, error = function(e) {
  cat("Task 1 results not available\n")
})

# Task 2: Regression Results
tryCatch({
  cat("\n--- TASK 2: REGRESSION ---\n")
  tar_load(regression_results)
  print(regression_results)
}, error = function(e) {
  cat("Task 2 results not available\n")
})

# Task 3: Unsupervised Learning Results
tryCatch({
  cat("\n--- TASK 3: UNSUPERVISED LEARNING ---\n")
  tar_load(unsupervised_results)
  print(unsupervised_results)
}, error = function(e) {
  cat("Task 3 results not available\n")
})

# Overall Summary
tryCatch({
  cat("\n--- OVERALL SUMMARY ---\n")
  tar_load(analysis_summary)
  print(analysis_summary)
}, error = function(e) {
  cat("Overall summary not available\n")
})

# Display and save plots
cat("\n=== PLOTS ===\n")

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Task 1: Classification Plots
tryCatch({
  cat("\n--- TASK 1: CLASSIFICATION PLOTS ---\n")
  tar_load(classification_plots)
  
  cat("Displaying target distribution plot...\n")
  print(classification_plots$target_dist)
  ggsave("plots/task1_target_distribution.png", classification_plots$target_dist, width = 10, height = 6)
  
  cat("Displaying feature importance plot...\n")
  print(classification_plots$feature_importance)
  ggsave("plots/task1_feature_importance.png", classification_plots$feature_importance, width = 10, height = 6)
  
  cat("✓ Classification plots saved to plots/ directory\n")
  
}, error = function(e) {
  cat("Classification plots not available\n")
})

# Task 2: Regression Plots
tryCatch({
  cat("\n--- TASK 2: REGRESSION PLOTS ---\n")
  tar_load(regression_plots)
  
  cat("Displaying regression interaction plot...\n")
  print(regression_plots$interaction)
  ggsave("plots/task2_regression_interaction.png", regression_plots$interaction, width = 10, height = 6)
  
  cat("✓ Regression plots saved to plots/ directory\n")
  
}, error = function(e) {
  cat("Regression plots not available\n")
})

# Task 3: Unsupervised Learning Plots
tryCatch({
  cat("\n--- TASK 3: UNSUPERVISED LEARNING PLOTS ---\n")
  tar_load(unsupervised_plots)
  
  cat("Displaying scree plot...\n")
  print(unsupervised_plots$scree)
  ggsave("plots/task3_scree_plot.png", unsupervised_plots$scree, width = 10, height = 6)
  
  cat("Displaying K-means clustering plot...\n")
  print(unsupervised_plots$kmeans_pca)
  ggsave("plots/task3_kmeans_clustering.png", unsupervised_plots$kmeans_pca, width = 10, height = 6)
  
  cat("✓ Unsupervised learning plots saved to plots/ directory\n")
  
}, error = function(e) {
  cat("Unsupervised learning plots not available\n")
})

# Load required test library
library(testthat)

# Run tests
cat("\n=== RUNNING TESTS ===\n")
tryCatch({
  test_results <- test_dir("tests/", reporter = "summary")
  cat("✓ All tests completed\n")
}, error = function(e) {
  cat("✗ Test errors occurred:", conditionMessage(e), "\n")
})

# Generate visualizations
cat("\n=== WORKFLOW VISUALIZATION ===\n")
tryCatch({
  cat("Target Status:\n")
  tar_glimpse()
  
  cat("\nTarget Progress:\n")
  tar_progress()
  
  cat("\nOutdated Targets:\n")
  outdated <- tar_outdated()
  if (length(outdated) > 0) {
    cat("Outdated:", paste(outdated, collapse = ", "), "\n")
  } else {
    cat("✓ All targets are up to date\n")
  }
  
}, error = function(e) {
  cat("✗ Visualization error:", conditionMessage(e), "\n")
})

cat("\n=== ANALYSIS COMPLETE ===\n") 