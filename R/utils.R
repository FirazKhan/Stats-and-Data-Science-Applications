# R/utils.R
# Utility functions for the analysis

#' Create analysis summary
#' @param classification_results Classification results
#' @param regression_results Regression results
#' @param unsupervised_results Unsupervised learning results
#' @return Summary data frame
create_analysis_summary <-
   function(classification_results, regression_results, unsupervised_results) {
  # Safe numerical operations with error handling
  rf_reduced_acc <- as.numeric(classification_results$rf_reduced_accuracy)
  dt_acc <- as.numeric(classification_results$dt_accuracy)
  feature_reduction <-
   as.numeric(classification_results$feature_reduction_percent)
  accuracy_loss <- as.numeric(classification_results$accuracy_loss_percent)

  # Handle potential NAs
  if (is.na(rf_reduced_acc)) rf_reduced_acc <- 0
  if (is.na(dt_acc)) dt_acc <- 0
  if (is.na(feature_reduction)) feature_reduction <- 0
  if (is.na(accuracy_loss)) accuracy_loss <- 0

  summary_df <- data.frame(
    Task = c("Task 1: Classification", "Task 2: Regression", "Task 3: Unsupervised"),
    Best_Model = c(
      ifelse(rf_reduced_acc > dt_acc, "Parsimonious Random Forest", "Decision Tree"),
      regression_results$best_model,
      "K-means Clustering"
    ),
    Performance_Metric = c(
      paste("Accuracy:", round(max(rf_reduced_acc, dt_acc), 4)),
      paste("AIC:", round(min(as.numeric(regression_results$aic_comparison$AIC)), 2), "- Pseudo R²:", round(regression_results$pseudo_r2, 3)),
      paste("Silhouette Score:", round(as.numeric(unsupervised_results$silhouette_kmeans), 4))
    ),
    Key_Finding = c(
      sprintf("Feature selection reduced complexity by %.1f%% with %.2f%% accuracy loss",
              feature_reduction, accuracy_loss),
      "Rate model with offset provides better interpretation of extrication likelihood per collision",
      "Natural groupings identified in olive oil composition data"
    )
  )

  summary_df
}

#' Print analysis summary
#' @param summary_df Summary data frame
print_analysis_summary <- function(summary_df) {
  cat("=== ANALYSIS SUMMARY ===\n")
  for (i in 1:nrow(summary_df)) {
    cat("\n", summary_df$Task[i], ":\n")
    cat("  Best Model:", summary_df$Best_Model[i], "\n")
    cat("  Performance:", summary_df$Performance_Metric[i], "\n")
    cat("  Key Finding:", summary_df$Key_Finding[i], "\n")
  }
  cat("\n=== END SUMMARY ===\n")
}

#' Validate data quality
#' @param data Data frame to validate
#' @param task_name Name of the task for reporting
validate_data_quality <- function(data, task_name) {
  cat("=== DATA QUALITY CHECK FOR", task_name, "===\n")
  cat("Dimensions:", dim(data), "\n")
  cat("Missing values:", sum(is.na(data)), "\n")
  cat("Complete cases:", sum(complete.cases(data)), "\n")

  if (sum(is.na(data)) > 0) {
    cat("Warning: Missing values detected\n")
  }

  if (nrow(data) == 0) {
    stop("Error: Empty dataset")
  }

  cat("Data quality check passed\n\n")
}

#' Save results to file
#' @param results Analysis results
#' @param filename Output filename
save_results <- function(results, filename) {
  saveRDS(results, file = filename)
  cat("Results saved to:", filename, "\n")
}

#' Load results from file
#' @param filename Input filename
#' @return Loaded results
load_results <- function(filename) {
  if (file.exists(filename)) {
    results <- readRDS(filename)
    cat("Results loaded from:", filename, "\n")
    return(results)
  } else {
    stop("File not found:", filename)
  }
}

