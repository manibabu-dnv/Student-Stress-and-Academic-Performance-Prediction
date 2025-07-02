install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
data <- read.csv("C:/Users/dnvma/Downloads/archive/student_lifestyle_dataset.csv")
data$Stress_Level <- as.factor(data$Stress_Level)
set.seed(123)
trainIndex <- createDataPartition(data$Stress_Level, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

model <- naiveBayes(Stress_Level ~ ., data = trainData) # Train the Naive Bayes model

predictions <- predict(model, testData)# Make predictions on the test set

conf_matrix <- confusionMatrix(predictions, testData$Stress_Level)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
print(paste("Accuracy:", accuracy))
