library(targets)
library(tarchetypes)
library(here)

# Source all R functions
source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))
source(here("R", "task1_classification.R"))
source(here("R", "task2_regression.R"))
source(here("R", "task3_unsupervised.R"))

# Set packages for all targets
tar_option_set(
  packages = c(
    "dplyr", "DBI", "RPostgres", "ggplot2", "tidyr",
    "caret", "randomForest", "rpart", "pROC",
    "mgcv", "stats", "cluster"
  )
)

list(
  # =============================================================================
  # DATA ACQUISITION TARGETS
  # =============================================================================
  
  # Raw data targets
  tar_target(
    pedestrian_data,
    load_pedestrian_data(),
    description = "Load pedestrian casualty data for Task 1"
  ),
  
  tar_target(
    fire_rescue_data,
    load_fire_rescue_data(),
    description = "Load fire rescue data for Task 2"
  ),
  
  tar_target(
    olive_oil_data,
    load_olive_oil_data(),
    description = "Load olive oil composition data for Task 3"
  ),
  
  # =============================================================================
  # TASK 1: SUPERVISED CLASSIFICATION TARGETS
  # =============================================================================
  
  tar_target(
    pedestrian_processed,
    preprocess_pedestrian_data(pedestrian_data),
    description = "Preprocess pedestrian data for classification"
  ),
  
  tar_target(
    pedestrian_split,
    split_pedestrian_data(pedestrian_processed),
    description = "Split pedestrian data into train/test sets"
  ),
  
  tar_target(
    rf_full_model,
    train_random_forest(pedestrian_split),
    description = "Train full Random Forest model for feature selection"
  ),
  
  tar_target(
    feature_selection_data,
    select_important_features(rf_full_model, pedestrian_split, top_n = 10),
    description = "Select top 10 most important features for parsimonious classifier"
  ),
  
  tar_target(
    rf_reduced_model,
    train_parsimonious_rf(feature_selection_data),
    description = "Train parsimonious Random Forest model with selected features"
  ),
  
  tar_target(
    dt_model,
    train_decision_tree(feature_selection_data),
    description = "Train Decision Tree model for comparison"
  ),
  
  tar_target(
    classification_results,
    evaluate_classification_models(rf_full_model, rf_reduced_model, dt_model, feature_selection_data),
    description = "Evaluate and compare classification models with feature selection analysis"
  ),
  
  # =============================================================================
  # TASK 2: REGRESSION TARGETS
  # =============================================================================
  
  tar_target(
    fire_rescue_processed,
    preprocess_fire_rescue_data(fire_rescue_data),
    description = "Preprocess fire rescue data for regression"
  ),
  
  tar_target(
    count_model,
    train_count_model(fire_rescue_processed),
    description = "Train Count Model (GLM without offset) for fire rescue analysis"
  ),
  
  tar_target(
    rate_model,
    train_rate_model(fire_rescue_processed),
    description = "Train Rate Model (GLM with offset) for fire rescue analysis"
  ),
  
  tar_target(
    gam_model,
    train_gam_model(fire_rescue_processed),
    description = "Train Generalized Additive Model for fire rescue analysis"
  ),
  
  tar_target(
    regression_results,
    evaluate_regression_models(count_model, rate_model, gam_model, fire_rescue_processed),
    description = "Evaluate regression models with comprehensive interpretation"
  ),
  
  # =============================================================================
  # TASK 3: UNSUPERVISED LEARNING TARGETS
  # =============================================================================
  
  tar_target(
    olive_oil_processed,
    preprocess_olive_oil_data(olive_oil_data),
    description = "Preprocess olive oil data for unsupervised learning"
  ),
  
  tar_target(
    pca_result,
    perform_pca(olive_oil_processed),
    description = "Perform Principal Component Analysis on olive oil data"
  ),
  
  tar_target(
    kmeans_result,
    perform_kmeans_clustering(olive_oil_processed),
    description = "Perform K-means clustering on olive oil data"
  ),
  
  tar_target(
    hclust_result,
    perform_hierarchical_clustering(olive_oil_processed),
    description = "Perform hierarchical clustering on olive oil data"
  ),
  
  tar_target(
    unsupervised_results,
    evaluate_unsupervised_models(pca_result, kmeans_result, hclust_result, olive_oil_processed),
    description = "Evaluate unsupervised learning models and generate results"
  ),
  
  # =============================================================================
  # VISUALIZATION TARGETS
  # =============================================================================
  
  tar_target(
    classification_plots,
    create_classification_plots(classification_results, pedestrian_processed),
    description = "Create plots for classification analysis"
  ),
  
  tar_target(
    regression_plots,
    create_regression_plots(regression_results, fire_rescue_processed),
    description = "Create plots for regression analysis"
  ),
  
  tar_target(
    unsupervised_plots,
    create_unsupervised_plots(unsupervised_results, pca_result, olive_oil_processed),
    description = "Create plots for unsupervised learning analysis"
  ),
  
  # =============================================================================
  # REPORT TARGETS
  # =============================================================================
  
  tar_render(
    report,
    "vignettes/Report.Rmd",
    description = "Generate final report with all analysis results"
  ),
  
  # =============================================================================
  # SUMMARY TARGETS
  # =============================================================================
  
  tar_target(
    analysis_summary,
    create_analysis_summary(
      classification_results,
      regression_results,
      unsupervised_results
    ),
    description = "Create summary of all analyses"
  )
)
