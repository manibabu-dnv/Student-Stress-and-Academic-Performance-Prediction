install.packages("neuralnet")
install.packages("tidyverse")
install.packages("readr")
install.packages("caret")
library(neuralnet)
library(tidyverse)
library(readr)
library(caret)

data <- read_csv("C:/Users/dnvma/Downloads/archive/student_lifestyle_dataset.csv")

data$Stress_Level <- as.factor(data$Stress_Level)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data <- data %>% mutate(across(where(is.numeric), normalize))
set.seed(123)
trainIndex <- sample(1:nrow(data), size = 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

colnames(trainData)# Verify column names
necessary_columns <- c("Study_Hours_Per_Day", "Extracurricular_Hours_Per_Day", "Sleep_Hours_Per_Day", "Social_Hours_Per_Day", "Physical_Activity_Hours_Per_Day", "GPA")

trainData <- trainData %>% select(all_of(necessary_columns))
testData <- testData %>% select(all_of(necessary_columns))

formula <- GPA ~ Study_Hours_Per_Day + Extracurricular_Hours_Per_Day + Sleep_Hours_Per_Day + Social_Hours_Per_Day + Physical_Activity_Hours_Per_Day

ann_model <- neuralnet(formula, data = trainData, hidden = c(5, 3), linear.output = TRUE)

plot(ann_model)

# Make predictions on the test set
testData_noGPA <- testData[, -which(names(testData) == "GPA")]
predictions <- predict(ann_model, testData_noGPA)

# Convert the predicted results back to the original scale
predictions <- predictions * (max(data$GPA) - min(data$GPA)) + min(data$GPA)

# Convert predictions to factor levels similar to GPA categories (example)
predicted_classes <- ifelse(predictions >= 0.75, "High", ifelse(predictions >= 0.5, "Moderate", "Low"))
actual_classes <- ifelse(testData$GPA >= 0.75, "High", ifelse(testData$GPA >= 0.5, "Moderate", "Low"))

confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
