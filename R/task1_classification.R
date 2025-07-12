# R/task1_classification.R
# Task 1: Supervised Classification Functions

library(RPostgres)
library(DBI)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(rpart)
library(pROC)

#' Load pedestrian casualty data
#' @return Data frame with pedestrian casualty data
load_pedestrian_data <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PGRDATABASE"),
    host = Sys.getenv("PGRHOST"),
    user = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    port = Sys.getenv("PGRPORT")
  )
  
  on.exit(dbDisconnect(con))
  
  casualties <- tbl(con, "stats19_casualties")
  accidents <- tbl(con, "stats19_accidents")
  vehicles <- tbl(con, "stats19_vehicles")
  
  # Feature Selection Strategy:
  # We select features based on domain knowledge and theoretical importance for pedestrian safety:
  # 1. Pedestrian characteristics: location, movement (behavioral factors)
  # 2. Environmental factors: light, weather, road conditions (visibility/hazard factors)
  # 3. Infrastructure: speed limit, road type, junction details (design factors)
  # 4. Vehicle factors: type, manoeuvre (impact characteristics)
  # 5. Demographic factors: age, sex (vulnerability factors)
  
  pedestrian_pipeline <- casualties %>%
    filter(casualty_class == "Pedestrian") %>%
    inner_join(accidents, by = "accident_index") %>%
    inner_join(vehicles, by = "accident_index") %>%
    select(
      casualty_severity,
      # Pedestrian behavior and location (high theoretical importance)
      pedestrian_location,
      pedestrian_movement,
      pedestrian_crossing_human_control,
      pedestrian_crossing_physical_facilities,
      
      # Environmental conditions (affect visibility and safety)
      light_conditions,
      weather_conditions,
      road_surface_conditions,
      
      # Infrastructure characteristics (design safety factors)
      speed_limit_mph,
      road_type,
      junction_detail,
      junction_control,
      urban_or_rural_area,
      
      # Vehicle factors (impact characteristics)
      vehicle_type,
      vehicle_manoeuvre,
      first_point_of_impact,
      
      # Demographic factors (vulnerability)
      age_of_casualty,
      sex_of_casualty,
      age_band_of_casualty,
      
      # Driver characteristics
      sex_of_driver,
      age_band_of_driver,
      
      # Context variables
      number_of_vehicles,
      number_of_casualties,
      first_road_class,
      age_of_vehicle
    )
  
  collect(pedestrian_pipeline)
}

#' Preprocess pedestrian data
#' @param data Raw pedestrian data
#' @return Preprocessed data
preprocess_pedestrian_data <- function(data) {
  # Convert character columns to factors
  data <- data %>%
    mutate(across(where(is.character), as.factor))
  
  # Handle missing values
  numeric_cols <- c("age_of_casualty", "number_of_vehicles", "number_of_casualties", 
                    "speed_limit_mph", "age_of_vehicle")
  
  for (col in numeric_cols) {
    if (any(is.na(data[[col]]))) {
      median_val <- median(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- median_val
    }
  }
  
  categorical_cols <- names(data)[sapply(data, is.factor)]
  categorical_cols <- categorical_cols[categorical_cols != "casualty_severity"]
  
  for (col in categorical_cols) {
    if (any(is.na(data[[col]]))) {
      mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      data[[col]][is.na(data[[col]])] <- mode_val
    }
  }
  
  data[complete.cases(data), ]
}

#' Split pedestrian data into train/test sets
#' @param data Preprocessed pedestrian data
#' @return List with train and test data
split_pedestrian_data <- function(data) {
  X <- data[, !names(data) %in% c("casualty_severity")]
  y <- data$casualty_severity
  
  set.seed(123)
  train_index <- createDataPartition(y, p = 0.8, list = FALSE)
  
  list(
    X_train = X[train_index, ],
    X_test = X[-train_index, ],
    y_train = y[train_index],
    y_test = y[-train_index]
  )
}

#' Train Random Forest model for feature selection
#' @param split_data Train/test split data
#' @return Trained Random Forest model with feature importance
#' @description Random Forest is chosen for this task because:
#' 1. Handles mixed data types (categorical and numeric) naturally
#' 2. Provides built-in feature importance measures
#' 3. Robust to outliers and missing values
#' 4. Captures non-linear relationships and interactions
#' 5. Less prone to overfitting compared to single decision trees
#' 6. Interpretable through feature importance and partial dependence plots
train_random_forest <- function(split_data) {
  set.seed(123)
  cat("Training Random Forest for feature selection...\n")
  cat("Random Forest is chosen because:\n")
  cat("- Handles mixed data types (categorical/numeric) naturally\n")
  cat("- Provides reliable feature importance measures\n")
  cat("- Robust to outliers and handles interactions well\n")
  cat("- Less prone to overfitting than single decision trees\n\n")
  
  randomForest(
    x = split_data$X_train,
    y = split_data$y_train,
    ntree = 300,
    importance = TRUE
  )
}

#' Select important features based on Random Forest importance
#' @param rf_model Trained Random Forest model
#' @param split_data Train/test split data
#' @param top_n Number of top features to select
#' @return List with selected features and reduced datasets
select_important_features <- function(rf_model, split_data, top_n = 10) {
  # Get feature importance
  importance_df <- data.frame(
    feature = rownames(importance(rf_model)),
    importance = importance(rf_model)[, "MeanDecreaseAccuracy"]
  )
  importance_df <- importance_df[order(-importance_df$importance), ]
  
  # Select top features
  top_features <- head(importance_df$feature, top_n)
  
  cat("Selected Top", top_n, "Features for Parsimonious Classifier:\n")
  for (i in 1:length(top_features)) {
    cat(sprintf("%d. %s (Importance: %.3f)\n", i, top_features[i], importance_df$importance[i]))
  }
  cat("\n")
  
  # Create reduced datasets
  X_train_reduced <- split_data$X_train[, top_features, drop = FALSE]
  X_test_reduced <- split_data$X_test[, top_features, drop = FALSE]
  
  list(
    top_features = top_features,
    importance_df = importance_df,
    X_train_reduced = X_train_reduced,
    X_test_reduced = X_test_reduced,
    X_train_full = split_data$X_train,
    X_test_full = split_data$X_test,
    y_train = split_data$y_train,
    y_test = split_data$y_test
  )
}

#' Train parsimonious Random Forest model
#' @param feature_selection_data Data with selected features
#' @return Trained parsimonious Random Forest model
train_parsimonious_rf <- function(feature_selection_data) {
  set.seed(123)
  cat("Training parsimonious Random Forest with selected features...\n")
  
  randomForest(
    x = feature_selection_data$X_train_reduced,
    y = feature_selection_data$y_train,
    ntree = 300,
    importance = TRUE
  )
}

#' Train Decision Tree model for comparison
#' @param feature_selection_data Data with selected features
#' @return Trained Decision Tree model
train_decision_tree <- function(feature_selection_data) {
  set.seed(123)
  cat("Training Decision Tree for comparison...\n")
  
  # Use reduced features for consistent comparison
  train_data <- data.frame(
    feature_selection_data$X_train_reduced, 
    casualty_severity = feature_selection_data$y_train
  )
  
  rpart(
    casualty_severity ~ .,
    data = train_data,
    method = "class"
  )
}

#' Evaluate classification models with feature selection analysis
#' @param rf_full_model Full Random Forest model
#' @param rf_reduced_model Parsimonious Random Forest model
#' @param dt_model Decision Tree model
#' @param feature_selection_data Data with selected features
#' @return List with comprehensive evaluation results
evaluate_classification_models <- function(rf_full_model, rf_reduced_model, dt_model, feature_selection_data) {
  cat("Evaluating models...\n")
  
  # Full RF predictions
  rf_full_pred <- predict(rf_full_model, feature_selection_data$X_test_full)
  rf_full_cm <- confusionMatrix(rf_full_pred, feature_selection_data$y_test)
  
  # Reduced RF predictions
  rf_reduced_pred <- predict(rf_reduced_model, feature_selection_data$X_test_reduced)
  rf_reduced_cm <- confusionMatrix(rf_reduced_pred, feature_selection_data$y_test)
  
  # Decision Tree predictions
  dt_pred <- predict(dt_model, feature_selection_data$X_test_reduced, type = "class")
  dt_cm <- confusionMatrix(dt_pred, feature_selection_data$y_test)
  
  # Calculate AUC for Random Forest models
  rf_full_prob <- predict(rf_full_model, feature_selection_data$X_test_full, type = "prob")
  rf_reduced_prob <- predict(rf_reduced_model, feature_selection_data$X_test_reduced, type = "prob")
  
  auc_results_full <- data.frame(Class = character(), AUC = numeric())
  auc_results_reduced <- data.frame(Class = character(), AUC = numeric())
  
  for (class_name in levels(feature_selection_data$y_test)) {
    binary_target <- as.numeric(feature_selection_data$y_test == class_name)
    
    # Full RF AUC
    full_class_prob <- rf_full_prob[, class_name]
    full_auc_value <- auc(roc(binary_target, full_class_prob))
    auc_results_full <- rbind(auc_results_full, data.frame(Class = class_name, AUC = full_auc_value))
    
    # Reduced RF AUC
    reduced_class_prob <- rf_reduced_prob[, class_name]
    reduced_auc_value <- auc(roc(binary_target, reduced_class_prob))
    auc_results_reduced <- rbind(auc_results_reduced, data.frame(Class = class_name, AUC = reduced_auc_value))
  }
  
  overall_auc_full <- mean(auc_results_full$AUC)
  overall_auc_reduced <- mean(auc_results_reduced$AUC)
  
  # Model comparison summary
  cat("\n=== MODEL COMPARISON SUMMARY ===\n")
  cat(sprintf("Full Random Forest (25 features): Accuracy = %.4f, AUC = %.4f\n", 
              rf_full_cm$overall["Accuracy"], overall_auc_full))
  cat(sprintf("Parsimonious Random Forest (10 features): Accuracy = %.4f, AUC = %.4f\n", 
              rf_reduced_cm$overall["Accuracy"], overall_auc_reduced))
  cat(sprintf("Decision Tree (10 features): Accuracy = %.4f\n", 
              dt_cm$overall["Accuracy"]))
  
  # Feature selection benefit
  feature_reduction <- (length(names(feature_selection_data$X_train_full)) - 
                       length(names(feature_selection_data$X_train_reduced))) / 
                       length(names(feature_selection_data$X_train_full)) * 100
  
  accuracy_loss <- (rf_full_cm$overall["Accuracy"] - rf_reduced_cm$overall["Accuracy"]) * 100
  
  cat(sprintf("\nFeature Selection Benefits:\n"))
  cat(sprintf("- Reduced features by %.1f%% (from %d to %d)\n", 
              feature_reduction, 
              length(names(feature_selection_data$X_train_full)),
              length(names(feature_selection_data$X_train_reduced))))
  cat(sprintf("- Accuracy loss: %.2f%%\n", accuracy_loss))
  cat(sprintf("- Model complexity significantly reduced\n"))
  cat(sprintf("- Improved interpretability and generalization potential\n\n"))
  
  list(
    # Full model results
    rf_full_accuracy = rf_full_cm$overall["Accuracy"],
    rf_full_kappa = rf_full_cm$overall["Kappa"],
    rf_full_confusion_matrix = rf_full_cm$table,
    overall_auc_full = overall_auc_full,
    auc_results_full = auc_results_full,
    
    # Reduced model results
    rf_reduced_accuracy = rf_reduced_cm$overall["Accuracy"],
    rf_reduced_kappa = rf_reduced_cm$overall["Kappa"],
    rf_reduced_confusion_matrix = rf_reduced_cm$table,
    overall_auc_reduced = overall_auc_reduced,
    auc_results_reduced = auc_results_reduced,
    
    # Decision tree results
    dt_accuracy = dt_cm$overall["Accuracy"],
    dt_kappa = dt_cm$overall["Kappa"],
    dt_confusion_matrix = dt_cm$table,
    
    # Feature importance and selection
    importance_df = feature_selection_data$importance_df,
    selected_features = feature_selection_data$top_features,
    
    # Model comparison metrics
    feature_reduction_percent = feature_reduction,
    accuracy_loss_percent = accuracy_loss,
    
    # Backward compatibility fields for existing report
    rf_accuracy = rf_reduced_cm$overall["Accuracy"],  # Use parsimonious model as main result
    rf_kappa = rf_reduced_cm$overall["Kappa"],
    rf_confusion_matrix = rf_reduced_cm$table,
    overall_auc = overall_auc_reduced,  # Use parsimonious model AUC
    auc_results = auc_results_reduced
  )
}

#' Create enhanced classification plots
#' @param results Classification results
#' @param data Processed data
#' @return List of plots
create_classification_plots <- function(results, data) {
  plots <- list()
  
  # Target variable distribution
  plots$target_dist <- ggplot(data, aes(x = casualty_severity, fill = casualty_severity)) +
    geom_bar() +
    labs(title = "Distribution of Pedestrian Casualty Severity",
         subtitle = "Understanding the class balance for classification",
         x = "Casualty Severity", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11))
  
  # Feature importance with selected features highlighted
  importance_plot_data <- head(results$importance_df, 15)
  importance_plot_data$selected <- ifelse(importance_plot_data$feature %in% results$selected_features, 
                                         "Selected", "Not Selected")
  
  plots$feature_importance <- ggplot(importance_plot_data, 
                                    aes(x = reorder(feature, importance), 
                                        y = importance, 
                                        fill = selected)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Selected" = "darkgreen", "Not Selected" = "lightblue")) +
    coord_flip() +
    labs(title = "Feature Importance Analysis",
         subtitle = "Top 15 features with selection for parsimonious classifier",
         x = "Feature", y = "Mean Decrease Accuracy",
         fill = "Selection Status") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11),
          legend.position = "bottom")
  
  plots
} 