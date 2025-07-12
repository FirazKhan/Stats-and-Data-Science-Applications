library(dplyr)

utils::globalVariables(c(
  "casualty_class",
  "casualty_severity",
  "pedestrian_location",
  "pedestrian_movement",
  "pedestrian_crossing_human_control",
  "pedestrian_crossing_physical_facilities",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "speed_limit_mph",
  "road_type",
  "junction_detail",
  "junction_control",
  "urban_or_rural_area",
  "vehicle_type",
  "vehicle_manoeuvre",
  "first_point_of_impact",
  "age_of_casualty",
  "sex_of_casualty",
  "age_band_of_casualty",
  "sex_of_driver",
  "age_band_of_driver",
  "number_of_vehicles",
  "number_of_casualties",
  "first_road_class",
  "age_of_vehicle",
  "sex",
  "financial_year",
  "n_casualties",
  "age_band",
  "mean_casualties",
  "mean_count",
  "mean_extrication_rate",
  "se_rate",
  "se_count",
  "kmeans_cluster",
  "hclust_cluster",
  "PC",
  "Variance",
  "Cumulative",
  "PC1",
  "PC2",
  "Cluster",
  "feature",
  "importance",
  "selected",
  "age_specific_casualties",
  "se_casualties"
))


utils::globalVariables(c(
  "casualty_class",
  "casualty_severity",
  "pedestrian_location",
  "pedestrian_movement",
  "pedestrian_crossing_human_control",
  "pedestrian_crossing_physical_facilities",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "speed_limit_mph",
  "road_type",
  "junction_detail",
  "junction_control",
  "urban_or_rural_area",
  "vehicle_type",
  "vehicle_manoeuvre",
  "first_point_of_impact",
  "age_of_casualty",
  "sex_of_casualty",
  "age_band_of_casualty",
  "sex_of_driver",
  "age_band_of_driver",
  "number_of_vehicles",
  "number_of_casualties",
  "first_road_class",
  "age_of_vehicle",
  "sex",
  "financial_year",
  "n_casualties",
  "age_band",
  "mean_casualties",
  "mean_count",
  "mean_extrication_rate",
  "se_rate",
  "se_count",
  "kmeans_cluster",
  "hclust_cluster",
  "PC",
  "Variance",
  "Cumulative",
  "PC1",
  "PC2",
  "Cluster",
  "feature",
  "importance",
  "selected",
  "age_specific_casualties",
  "se_casualties"
))

utils::globalVariables(c("casualty_severity"))

summarise_data <- function(df) {
  df %>%
    dplyr::mutate(casualty_severity = as.factor(casualty_severity)) %>% # nolint
    dplyr::group_by(casualty_severity) %>%
    dplyr::summarise(Casualties = n())
}

summarise_casualty_sex <- function(df) {
  df %>%
    dplyr::mutate(sex_of_casualty = as.factor(sex_of_casualty)) %>% # nolint
    dplyr::group_by(sex_of_casualty) %>%
    dplyr::summarise(Casualties = n())
}

