# -----------------------------------------------------------------------------
# This code snippet was generated for educational purposes as part of the
# Intelligent Systems course at the University of Deusto. The code has been
# created with assistance from ChatGPT version 3.5 and GitHub Copilot.
#
# The code is released under the Creative Commons License and is provided
# for free use and modification by the programming and development community.
#
# This script was generated in April 2024, the year when the Athletic Club de
# Bilbao won the 25th King's Cup.
# -----------------------------------------------------------------------------

# Install required packages
library(lattice)
library(ggplot2)
library(caret)

# Clear console
cat("\014")
# Clear plots
if(!is.null(dev.list())) dev.off()
graphics.off()
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom functions
source("linear-regression-utils.R")

#-----------------------
# READ AND PREPARE DATA
#-----------------------
data <- read.csv("../data/2023_song_data.csv")

# Name is omitted because it is not a numerical value.
data$song_name <- NULL

# Song duration is omitted because it is not a relevant value.
#data$song_duration_ms <- NULL

# Key is omitted because it is not a relevant value.
#data$key <- NULL

# Audio mode is omitted because it is not a relevant value.
#data$audio_mode <- NULL

# Speechiness is omitted because it is not a relevant value.
#data$speechiness <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "song_popularity")
# Plot data distribution
plot.data.distribution(data, "song_popularity")

#-----------------------------------
# GENERATA AND ANALYZE LINEAR MODEL
#-----------------------------------
# Initialize variables
total_avg_error <- 0
best_model      <- NULL
min_avg_error   <- max(data$song_popularity)
training_p      <- 0.70

# Repeat the process 10 times
for (i in 1:10) {
  # Generate data partition 70% training / 30% test. The result is a vector with
  # the indexes of the examples that will be used for the training of the model.
  training_samples <- createDataPartition(y = data$song_popularity, p = training_p, list = FALSE)
  # Split training and test data
  training_data    <- data[training_samples, ]
  test_data        <- data[-training_samples, ]
  # Create Linear Model using training data. Formula = all the columns except song_popularity
  model <- lm(formula = data$song_popularity ~., data = data)
  # Make the prediction using the model and test data
  prediction       <- predict(model, test_data)
  # Calculate Mean Average Error
  mean_avg_error   <- mean(abs(prediction - test_data$song_popularity))
  # Print Mean Absolute Error
  print(paste0("- Mean average error of model'", i, ": ", mean_avg_error))
  # Update the best model and the minimum average error
  if (mean_avg_error < min_avg_error) {
    min_avg_error <- mean_avg_error
    best_model <- model
  }
  # Update the total average error
  total_avg_error <- total_avg_error + mean_avg_error
}

# Calculate the total average error
total_avg_error <- (total_avg_error / 10)
# Print total and best average error
print(paste0("- Total average error: ", total_avg_error))
print(paste0("- Best average error: ", min_avg_error))

# Print standard summary of the best model
summary(best_model)
# Print summary of the best model
print.model.sumary(best_model)

# Make the prediction using the model and test data
prediction       <- predict(best_model, test_data)
# Calculate de prediction error
prediction_error <- abs(prediction - test_data$song_popularity)
# Obtain the index of the example with the highest error
index_max_error  <- which.max(prediction_error)
# Print the example with the highest error
print(test_data[index_max_error, ])
# Print the error of the example with the highest error
print(paste0("Real song_popularity: ", round(test_data$song_popularity[index_max_error], 4),
             " / Prediction: ", round(prediction[index_max_error], 4),
             " / inc.: ", round(prediction_error[index_max_error], 4)))




# -----------------------------
# ANSWER THE QUESTIONS
# -----------------------------

# Generate predictions with altered speechiness and acousticness
increase_factor <- 0.1

# Function to get predictions with altered variable
get_altered_predictions <- function(model, data, variable, increase) {
  altered_data <- data
  altered_data[[variable]] <- altered_data[[variable]] + increase
  predict(model, altered_data)
}

# Get original predictions
original_predictions <- predict(best_model, data)

# Increase speechiness by 0.1 and get new predictions
speechiness_predictions <- get_altered_predictions(best_model, data, "speechiness", increase_factor)
speechiness_increase <- speechiness_predictions - original_predictions

# Increase acousticness by 0.1 and get new predictions
acousticness_predictions <- get_altered_predictions(best_model, data, "acousticness", increase_factor)
acousticness_increase <- acousticness_predictions - original_predictions

# Get the 10 songs with the highest increase in predictions for speechiness
top_speechiness_increase <- order(speechiness_increase, decreasing = TRUE)[1:10]
cat("Top 10 songs with highest increase in prediction by increasing speechiness by 0.1:\n")
print(data[top_speechiness_increase, ])

# Get the 10 songs with the highest increase in predictions for acousticness
top_acousticness_increase <- order(acousticness_increase, decreasing = TRUE)[1:10]
cat("Top 10 songs with highest increase in prediction by increasing acousticness by 0.1:\n")
print(data[top_acousticness_increase, ])

# Find the 10 least popular songs
least_popular_indices <- order(data$song_popularity)[1:10]
least_popular_songs <- data[least_popular_indices, ]

# Calculate the impact of increasing each variable by 0.1 for the least popular songs
variables <- setdiff(names(data), "song_popularity")
impact_matrix <- matrix(0, nrow = length(least_popular_indices), ncol = length(variables))
colnames(impact_matrix) <- variables

for (var in variables) {
  altered_predictions <- get_altered_predictions(best_model, least_popular_songs, var, increase_factor)
  impact_matrix[, var] <- altered_predictions - original_predictions[least_popular_indices]
}

# Find the variable with the highest average impact
average_impact <- colMeans(impact_matrix)
most_impactful_variable <- names(which.max(average_impact))

cat("The variable that most impacts popularity for the 10 least popular songs if increased by 0.1 is:", most_impactful_variable, "\n")
cat("Average impact of increasing", most_impactful_variable, "by 0.1:", average_impact[most_impactful_variable], "\n")
