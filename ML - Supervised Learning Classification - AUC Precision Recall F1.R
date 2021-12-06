#install packages
install.packages('MLmetrics')

#1.train model with prSummary
#precision, recal, F1, AUC
set.seed(27)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE,
                     summaryFunction =  prSummary,
                     classProbs = TRUE)

knn_model <- train(Class ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "Recall",
                   trControl = ctrl)

#2.score (prediction)
knn <- predict(knn_model, newdata = test_data)

#3.evaluate model performance
knn_rmse <- mean(knn == test_data$Class)

#4.Confusion Matrix
cm <- table(knn, 
            test_data$Class,
            dnn = c('Prediction', 'Actual'))

confusionMatrix(knn, test_data$Class,
                positive = 'R',
                mode = 'prec_recall')
