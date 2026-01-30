# Q3
library(e1071)
# fit the SVM model with default settings
svm_model <- svm(credit.rating ~ ., data = train_set, probability = TRUE)
# print the model summary
summary(svm_model)

# Q3.a
# predict the credit rating for the median customer using the SVM model
median_customer_prediction <- predict(svm_model, newdata = median_customer, decision.values = TRUE)
# print the result of the SVM model predicted
print(median_customer_prediction)
print(paste("Predicted for median customer: ", median_customer_prediction))

# Q3.b
# predict on test set
svm_test_prediction <- predict(svm_model, newdata = test_set, probability = TRUE)
# create the confusion matrix for the test set
svm_test_confusion_matrix <- table(test_set$credit.rating, svm_test_prediction)
# print the result for the confusion matrix on the test set
print(svm_test_confusion_matrix)
# calculate the overall accuracy for thr test set
svm_test_acc <- sum(diag(svm_test_confusion_matrix)) / sum(svm_test_confusion_matrix)
# print the overall acc for test set
print(paste("the overall test accuracy (SVM): ", round(svm_test_acc, 4)))

# Q3.c
train_set$credit.rating <- as.factor(train_set$credit.rating)
# automatically parameters adjustment by adjusting cost and gamma
tune_SVM <- tune(svm, credit.rating ~ ., 
                 data = train_set, 
                 kernel = "radial", 
                 type = "C-classification", 
                 ranges = list(cost = c(0.1, 1, 10, 50), gamma = c(0.01, 0.1, 1, 5))
)
# retrieve the best model
best_SVM_model <- tune_SVM$best.model
best_SVM_model
best_SVM_model$cost
best_SVM_model$gamma
# using tunned model to test the test set
tuned_prediction <- predict(best_SVM_model, newdata = test_set)
# create the confusion matrix
tuned_cm <- table(test_set$credit.rating, tuned_prediction)
print(tuned_cm)
# claculate accuracy
tuned_acc <- mean(tuned_prediction == test_set$credit.rating)
cat("Tuned SVM accuracy: ", round(tuned_acc * 100, 2), "%")
