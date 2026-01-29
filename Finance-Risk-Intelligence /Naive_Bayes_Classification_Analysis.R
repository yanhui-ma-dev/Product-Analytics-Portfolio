# Q4.b
# fit the naive bayes model
library(e1071)
library(caret)
# read csv
data <- read.csv("creditworthiness.csv")
# convert all cols to factors
category_cols <- names(data)
data[category_cols] <- lapply(data[category_cols], as.factor)
# filter ppl with credit rating
filtered_data <- data[data$credit.rating != 0,]
filtered_data$credit.rating <- droplevels(filtered_data$credit.rating)
# set seed to manke sure results can repeat
set.seed(123)
# assign variable n
n <- nrow(filtered_data)
# random sample 50% as train index
train_index <- sample(1:n, size = floor(0.5*n))
# set training and test set
train_set <- filtered_data[train_index, ]
test_set <- filtered_data[-train_index, ]
# ensure credit.rating as a factor for classification
train_set$credit.rating <- as.factor(train_set$credit.rating)
test_set$credit.rating <- as.factor(test_set$credit.rating)
# fit the navie bayes model
nb_model <- naiveBayes(credit.rating ~ ., data = train_set)
# print the model summary
summary(nb_model)
print(nb_model)

# Q4.a
# predict the credit rating and probabilities for the median customer
predicted_rating <- predict(nb_model, newdata = median_customer)
warnings(predicted_rating)
predicted_probabilities <- predict(nb_model, newdata = median_customer, type = "raw")
# print the predicted results
print(paste("Predicted credit rating for median customer: ", predicted_rating))
print("Predicted probabilities for median customer: ")
print(predicted_probabilities)

# Q4.c
# predict on the test set with the naive bayes model
nb_prediction <- predict(nb_model, newdata = test_set)
# create confusion matrix for the test
nb_cm <- table(test_set$credit.rating, nb_prediction)
# print the confusion matrix for the test
print("Confusion mactrix for the test set (NB): ")
print(nb_cm)
# calculate overall accuracy for th test
nb_acc <- sum(diag(nb_cm)) / sum(nb_cm)
# print the overall accuracy for test set
print(paste("Overall accuracy for the test set(NB): ", round(nb_acc, 4)))
confusion_stats <- confusionMatrix(nb_cm)
print(confusion_stats)
