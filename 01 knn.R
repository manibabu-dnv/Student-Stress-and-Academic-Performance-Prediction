install.packages("class")
install.packages("dplyr")
install.packages("readr")
library(class)
library(dplyr)
library(readr)
data <- read.csv("C:/Users/dnvma/Downloads/archive/student_lifestyle_dataset.csv")
head(data)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_normalized <- data %>%
  mutate(across(where(is.numeric), normalize))

data_normalized$Stress_Level <- factor(data_normalized$Stress_Level)
set.seed(123) #reproducible or not

train_indices <- sample(1:nrow(data_normalized), size = 0.7 * nrow(data_normalized))
train_data <- data_normalized[train_indices, ]
test_data <- data_normalized[-train_indices, ]

train_labels <- train_data$Stress_Level
test_labels <- test_data$Stress_Level

train_data <- train_data[, -which(names(train_data) == "Stress_Level")]
test_data <- test_data[, -which(names(test_data) == "Stress_Level")]
k <- 3 

predicted_labels <- knn(train = train_data, test = test_data, cl = train_labels, k = k)

confusion_matrix <- table(Predicted = predicted_labels, Actual = test_labels)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))