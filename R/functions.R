library(dplyr)
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

