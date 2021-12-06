#load library
library(caret)
library(tidyverse)
library(mlbench)

#load data set
data('Sonar')

#preview data set
view(Sonar)
glimpse(Sonar)

#check missing value?
mean(complete.cases(Sonar)) # if show 1 data set is complete value

#review column class
table(Sonar$Class)

#1.split data
set.seed(27)
id <- createDataPartition(y = Sonar$Class,
                    p = 0.8,
                    list = FALSE)

train_data <- Sonar[id, ] #train data 80%
test_data <- Sonar[-id, ] #test data 20%

#2.train model
set.seed(27)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

knn_model <- train(Class ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "Accuracy",
                   trControl = ctrl)

tree_model <- train(Class ~ .,
                   data = train_data,
                   method = "rpart",
                   metric = "Accuracy",
                   trControl = ctrl)

randomForrest_model <- train(Class ~ .,
                    data = train_data,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = ctrl)

#3.score (prediction)
knn <- predict(knn_model, newdata = test_data)
tree <- predict(tree_model, newdata = test_data)
randomForrest <- predict(randomForrest_model, newdata = test_data)

#4.evaluate model performance
knn_rmse <- mean(knn == test_data$Class)
tree_rmse <- mean(tree == test_data$Class)
randomForrest_rmse <- mean(randomForrest == test_data$Class)

#5.model comparison
modelList <- list(
  kNN = knn_model,
  dicissionTree = tree_model,
  rdmForrest = randomForrest_model
)

result <- resamples(modelList)
summary(result)