# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom functions
source("decision-tree-learning-utils.R")

#-----------------------
# LOAD AND PREPARE DATA
#-----------------------
# Read data from CSV
filename = "../data/2024_clients-train.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

filename = "../data/2024_clients-test.csv"
dataR <- read.csv(file = filename, sep =",", header = TRUE)

data <- rbind(data, dataR);

#------------------------
# PLOT DATA DISTRIBUTION
#------------------------

# Folder to save images
images_folder <- "../data/images/2024-clients-train/"
# Plot the relation between the target variable and the rest of the columns
plot.data.distribution(data, target = "Segmentation", folder=images_folder)

# Create 4 bims for Age (based on lowest and highest values)
bin_width <- round((max(data$Age) - min(data$Age)) / 4)
labels <- paste0(min(data$Age) + (1:4 - 1) * bin_width, " <-> ", min(data$Age) + 1:4 * bin_width)
data$Age <- cut(data$Age, breaks = 4, labels = labels)
# Create 3 bims for Work_Experience (Low, Medium, High)
data$Work_Experience <- ifelse(data$Work_Experience < 3, "Low (<3)",
                         ifelse(data$Work_Experience <= 9, "Medium (>=3 && <=9)", "High (>10)"))
# Create 3 bims for RestingBP (Normal, Medium, Large, very Large)
data$Family_Size <- ifelse(data$Family_Size < 1, "Normal (<1)",
                         ifelse(data$Family_Size <= 2, "Medium (>=2 && <=3)",
                         ifelse(data$Family_Size <= 3, "Large (>=3 && <=4)", "Very Large (>5)")))

#---------------------
# PLOT DATA FREQUENCY
#---------------------
# Plot the frequency of each attribute
plot.data.frequency(data, folder=images_folder)

#---------------------------------------------------
# GENERATE AND ANALYZE DECISION TREE LEARNING MODEL
#---------------------------------------------------
# Percentage of training examples
training_p <- 0.8

best_model <- NA
best_accuracy <- 0

for(i in 1:10) {
  # Generate data partition 80% training / 20% test. The result is a vector with
  # the indexes of the examples that will be used for the training of the model.
  training_indexes <- createDataPartition(y = data$Segmentation, p = training_p, list = FALSE)

  # Split training and test data
  training_data <- data[training_indexes, ]
  test_data     <- data[-training_indexes, ]

  # Create Linear Model using training data. Formula = all the columns except Salary
  model <- rpart(formula = Segmentation~., data = training_data)

  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")

  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$Segmentation, prediction)
  matrix <- confusionMatrix(prediction_results)
  accuracy <- matrix$overall[1]
  # Get attribute names order by importance
  attr <- names(model$variable.importance)

  # Print accuracy and top 5 attributes
  cat(paste0(i, ".- Accuracy = ", round(100*accuracy, digits = 2)),
      "%. Top 5 attributes:\n")
  # Print top 5 attributes
  for (j in 1:5) {
    print(paste0("    Attribute-", j, " = ", attr[j]), quote = FALSE)
  }

  # Update best model if accuracy is better
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
  }
}

# Print accurcy and attributes in order of relevance
attrs <- names(best_model$variable.importance)
cat(paste0("Accuracy = ", round(100*best_accuracy, digits = 2)),
    "%\nAttributes ordered by relevance:", paste0("\n  ", attrs))

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(best_model,
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "BuGn",
           shadow.col = "darkgray",
           main = "Segmentation Decision Tree",
           sub = paste0("Accuracy = ", round(100*best_accuracy, digits = 2), "%"))
# Print the rules that represent the Decision Tree
rpart.rules(model,
            style="tall",
            trace = 0,
            cover = TRUE,
            eq = "==",
            when = "IF",
            and = "&&",
            extra = 4)

#Pregunta 1: Que pasa si todo el mundo tiene estudios superiores
dataNewGraduated <- data
#Cambiamos la variable graduado a Si
dataNewGraduated$Graduated <- "Yes"

prediction <- predict(best_model, data, type = "class")
predictionNewGraduated <- predict(best_model, dataNewGraduated, type = "class")

segmentationChanged <- prediction != predictionNewGraduated

if (any(segmentationChanged)) {
  ant_segmentacion <- data$Segmentation[segmentationChanged]
  nueva_segmentacion <- prediction_new[segmentationChanged]
  
  cat("Antigua Segmentacion:", ant_segmentacion, "\n")
  
  cat("Nueva Segmentatcion:", nueva_segmentacion, "\n")
} else {
   print("Sin cambios en la segmentacion")
}



#Pregunta 2: Que pasa si todo el mundo tiene o ha tenido matrimonio

dataNewGraduated <- data
#Cambiamos la variable Casado a Si
dataNewGraduated$Ever_Married <- "Yes"

prediction <- predict(best_model, data, type = "class")
predictionNewGraduated <- predict(best_model, dataNewGraduated, type = "class")

segmentationChanged <- prediction != predictionNewGraduated

if (any(segmentationChanged)) {
  ant_segmentacion <- data$Segmentation[segmentationChanged]
  nueva_segmentacion <- prediction_new[segmentationChanged]
  
  cat("Antigua Segmentacion:", ant_segmentacion, "\n")
  
  cat("Nueva Segmentatcion:", nueva_segmentacion, "\n")
} else {
  print("Sin cambios en la segmentacion")
}


