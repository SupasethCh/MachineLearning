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

randomForrest_model <- train(Class ~ .,
                             data = train_data,
                             method = "rf",
                             metric = "Accuracy",
                             trControl = ctrl)

#3.score (prediction)
randomForrest <- predict(randomForrest_model, newdata = test_data)

#4.evaluate model performance
randomForrest_rmse <- mean(randomForrest == test_data$Class)

#Confusion Matrix - table(prediction, actual)
cm <- table(randomForrest, 
      test_data$Class,
      dnn = c('Prediction', 'Actual'))

#accuracy
(20 + 16) / (20 + 3 + 2 + 16)
accuracy <- sum(diag(cm)) / sum(cm)

#precision => 'M' positive class
20 / (20 + 3)
precision <- cm[1, 1] / sum(cm[1, ])

#recall
20 / (20 + 2)
recall <- cm[1, 1] / sum(cm[ , 1])

#F1
F1 <- 2 * (precision * recall) / (precision + recall) 

##Confusion Matrix (Binary Classification) 
confusionMatrix(randomForrest, test_data$Class,
                positive = 'R')
