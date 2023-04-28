#' Calculate fitness metrics based on a data frame of fitness data
#'
#' This package provides functions to calculate various fitness metrics based on a data frame of fitness data.
#'
#' @param data A data frame containing the fitness data.
#'
#' @importFrom stats sapply
#' @importFrom utils stop
#' @importFrom base sum
#'
#' @return A list containing the calculated fitness metrics.
#'
#' @export

#' Calculate the total step count from fitness data
#'
#' This function calculates the total step count from a data frame of fitness data.
#'
#' @importFrom base sum
#' @importFrom utils is.na
#'
#' @return The total step count from the fitness data.
#'
#' @export
step_count <- function(data) {
  sum(data$steps[!is.na(data$steps)]) # Only sum steps with non-missing values
}

#' Calculate the total distance traveled from fitness data
#'
#' This function calculates the total distance traveled from a data frame of fitness data.
#'
#' @importFrom base sum
#' @importFrom utils is.na
#'
#' @return The total distance traveled from the fitness data.
#'
#' @export
distance_traveled <- function(data) {
  sum(data$distance[!is.na(data$distance)]) # Only sum distances with non-missing values
}

#' Calculate the total calories burned from fitness data
#'
#' This function calculates the total calories burned from a data frame of fitness data.
#'
#' @param data A data frame containing the fitness data.
#' @param gender The gender of the person.
#' @param age The age of the person.
#' @param weight The weight of the person in Kg.
#' @param height The height of the person in Cm.
#'
#' @importFrom base sum
#' @importFrom utils is.na
#' @importFrom stats sapply
#'
#' @return The total calories burned from the fitness data.
#'
#' @export
calories_burned <- function(data, gender, age, weight, height) {
  if (gender == "Male") {
    bmr <- 88.362 + (13.397 * weight) + (4.799 * height) - (5.677 * age)
  } else {
    bmr <- 447.593 + (9.247 * weight) + (3.098 * height) - (4.330 * age)
  }
  calories_burned <- sapply(data$steps, function(steps) {
    step_calories(steps, weight)
  }) * bmr * 1.2
  sum(calories_burned[!is.na(calories_burned)]) # Only sum calories with non-missing values
}

#' Check and return fitness metrics
#'
#' This function checks that a data frame contains the necessary columns for calculating fitness metrics, and returns the calculated fitness metrics.
#'
#' @importFrom utils stop
#' @importFrom base names all
#'
#' @return A list containing the calculated fitness metrics.
#'
#' @export
get_fitness_metrics <- function(data) {
  
  # Define a function to check that the data frame has the necessary columns
  check_columns <- function(data, columns) {
    all(sapply(columns, function(col) col %in% names(data)))
  }
  
  # Check that the fitness data has the necessary columns
  required_cols <- c("steps", "distance", "gender", "age", "weight", "height")
  if (!check_columns(data, required_cols)) {
    stop("Fitness data is missing required columns.")
  }
  
  # Check for missing values in gender, age, weight, and height
  if (any(is.na(data[c("gender", "age", "weight", "height")]))) {
    stop("Gender, age, weight, or height is missing.")
  }
  
  # Return the fitness metrics
  list(
    step_count = sum(data$steps),
    distance_traveled = sum(data$distance),
    calories_burned = calories_burned(data$gender, data$age, data$weight, data$height, sum(data$steps))
  )
}
