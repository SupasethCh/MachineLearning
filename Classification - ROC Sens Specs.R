#1.train model with prSummary
#ROC, Sens, Specs
set.seed(27)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE,
                     summaryFunction =  twoClassSummary,
                     classProbs = TRUE)

randomForrest_model <- train(Class ~ .,
                   data = train_data,
                   method = "rf",
                   metric = "ROC",
                   trControl = ctrl)

#2.score (prediction)
randomForrest <- predict(randomForrest_model, newdata = test_data)

#3.evaluate model performance
randomForrest_rmse <- mean(randomForrest == test_data$Class)

#4.Confusion Matrix
cm <- table(randomForrest, 
            test_data$Class,
            dnn = c('Prediction', 'Actual'))

confusionMatrix(randomForrest, test_data$Class,
                positive = 'M',
                )