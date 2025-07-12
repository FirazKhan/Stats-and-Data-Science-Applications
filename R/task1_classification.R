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
  
  pedestrian_pipeline <- casualties %>%
    filter(casualty_class == "Pedestrian") %>%
    inner_join(accidents, by = "accident_index") %>%
    inner_join(vehicles, by = "accident_index") %>%
    select(
      casualty_severity,
      sex_of_casualty,
      age_of_casualty,
      age_band_of_casualty,
      pedestrian_location,
      pedestrian_movement,
      accident_severity,
      number_of_vehicles,
      number_of_casualties,
      first_road_class,
      road_type,
      speed_limit_mph,
      junction_detail,
      junction_control,
      pedestrian_crossing_human_control,
      pedestrian_crossing_physical_facilities,
      light_conditions,
      weather_conditions,
      road_surface_conditions,
      special_conditions_at_site,
      carriageway_hazards,
      urban_or_rural_area,
      vehicle_type,
      vehicle_manoeuvre,
      first_point_of_impact,
      sex_of_driver,
      age_band_of_driver,
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

#' Train Random Forest model
#' @param split_data Train/test split data
#' @return Trained Random Forest model
train_random_forest <- function(split_data) {
  set.seed(123)
  randomForest(
    x = split_data$X_train,
    y = split_data$y_train,
    ntree = 300,
    importance = TRUE
  )
}

#' Train Decision Tree model
#' @param split_data Train/test split data
#' @return Trained Decision Tree model
train_decision_tree <- function(split_data) {
  set.seed(123)
  rpart(
    casualty_severity ~ .,
    data = data.frame(split_data$X_train, casualty_severity = split_data$y_train),
    method = "class"
  )
}

#' Evaluate classification models
#' @param rf_model Random Forest model
#' @param dt_model Decision Tree model
#' @param split_data Train/test split data
#' @return List with evaluation results
evaluate_classification_models <- function(rf_model, dt_model, split_data) {
  # Make predictions
  rf_pred <- predict(rf_model, split_data$X_test)
  dt_pred <- predict(dt_model, split_data$X_test, type = "class")
  
  # Create confusion matrices
  rf_cm <- confusionMatrix(rf_pred, split_data$y_test)
  dt_cm <- confusionMatrix(dt_pred, split_data$y_test)
  
  # Calculate AUC for Random Forest
  rf_prob <- predict(rf_model, split_data$X_test, type = "prob")
  auc_results <- data.frame(Class = character(), AUC = numeric())
  
  for (class_name in levels(split_data$y_test)) {
    binary_target <- as.numeric(split_data$y_test == class_name)
    class_prob <- rf_prob[, class_name]
    auc_value <- auc(roc(binary_target, class_prob))
    auc_results <- rbind(auc_results, data.frame(Class = class_name, AUC = auc_value))
  }
  
  overall_auc <- mean(auc_results$AUC)
  
  # Feature importance
  importance_df <- data.frame(
    feature = rownames(importance(rf_model)),
    importance = importance(rf_model)[, "MeanDecreaseAccuracy"]
  )
  importance_df <- importance_df[order(-importance_df$importance), ]
  
  list(
    rf_accuracy = rf_cm$overall["Accuracy"],
    dt_accuracy = dt_cm$overall["Accuracy"],
    rf_kappa = rf_cm$overall["Kappa"],
    dt_kappa = dt_cm$overall["Kappa"],
    overall_auc = overall_auc,
    auc_results = auc_results,
    importance_df = importance_df,
    rf_confusion_matrix = rf_cm$table,
    dt_confusion_matrix = dt_cm$table
  )
}

#' Create classification plots
#' @param results Classification results
#' @param data Processed data
#' @return List of plots
create_classification_plots <- function(results, data) {
  plots <- list()
  
  # Target variable distribution
  plots$target_dist <- ggplot(data, aes(x = casualty_severity, fill = casualty_severity)) +
    geom_bar() +
    labs(title = "Distribution of Pedestrian Casualty Severity",
         x = "Casualty Severity", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"))
  
  # Feature importance
  plots$feature_importance <- ggplot(head(results$importance_df, 15), 
                                    aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Top 15 Most Important Features (Random Forest)",
         x = "Feature", y = "Importance") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots
} 