install.packages("rpart")
install.packages("rpart.plot")
install.packages("readr")
library(rpart)
library(rpart.plot)
library(readr)
data <- read.csv("C:/Users/dnvma/Downloads/archive/student_lifestyle_dataset.csv")

data$Stress_Level <- as.factor(data$Stress_Level)
set.seed(123)
trainIndex <- sample(1:nrow(data), size = 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

model <- rpart(Stress_Level ~ ., data = trainData, method = "class")

rpart.plot(model, main="Decision Tree for Student Stress Levels")

predictions <- predict(model, testData, type = "class")

confusion_matrix <- table(Predicted = predictions, Actual = testData$Stress_Level)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
