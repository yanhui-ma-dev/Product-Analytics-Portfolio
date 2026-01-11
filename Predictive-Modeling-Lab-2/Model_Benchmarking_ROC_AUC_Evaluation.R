# Q6 predicting A rating
library(e1071)
library(pROC)
# load and prepare data
data <- read.csv("creditworthiness.csv")
# Filter out samples without credit rating
filtered_data <- subset(data, credit.rating != 0)
# 50/50 train-test split
set.seed(123)
n <- nrow(filtered_data)
train_indices <- sample(1:n, size = floor(0.5 * n))
train_set <- filtered_data[train_indices, ]
test_set <- filtered_data[-train_indices, ]
# automatically identify predictor types
predictor_names <- setdiff(names(train_set), "credit.rating")
numeric_columns <- predictor_names[sapply(train_set[predictor_names], is.numeric)]
categorical_columns <- setdiff(predictor_names, numeric_columns)
# convert categorical predictors to factor
train_set[categorical_columns] <- lapply(train_set[categorical_columns], as.factor)
test_set[categorical_columns]  <- lapply(test_set[categorical_columns], as.factor)
# convert target variable to binary factor: 1 = A, 0 = not A
train_set$credit.rating <- factor(ifelse(train_set$credit.rating == 1, 1, 0), levels = c(0, 1))
test_set$credit.rating  <- factor(ifelse(test_set$credit.rating == 1, 1, 0), levels = c(0, 1))


# Q6.a fit logistic regression
log_model <- glm(credit.rating ~ ., data = train_set, family = binomial("logit"))

# Q6.b report model summary
summary(log_model)

# Q6.c identify significant predictors at 5% level
p_values <- summary(log_model)$coefficients[, 4]
significant_predictors <- names(p_values[p_values < 0.05])
print("Significant predictors at 5% level:")
print(significant_predictors)


# Q6.d fit SVM model
svm_model <- svm(credit.rating ~ ., data = train_set, kernel = "linear", probability = TRUE)
summary(svm_model)

# Q6.e compare ROC curves
# logistic regression predictions
log_probs <- predict(log_model, newdata = test_set, type = "response")
# SVM predictions with probabilities
svm_pred <- predict(svm_model, newdata = test_set, probability = TRUE)
svm_probs <- attr(svm_pred, "probabilities")[, "1"]  # Probability of class 1 = A rating
# ROC and AUC calculation
roc_log <- roc(test_set$credit.rating, log_probs)
roc_svm <- roc(test_set$credit.rating, svm_probs)

auc_log <- auc(roc_log)
auc_svm <- auc(roc_svm)
# plot ROC curves
plot(roc_log, col = "blue", main = "ROC Curve: Logistic vs SVM")
plot(roc_svm, col = "red", add = TRUE)
legend("bottomright",
       legend = c(paste("Logistic AUC =", round(auc_log, 4)),
                  paste("SVM AUC =", round(auc_svm, 4))),
       col = c("blue", "red"), lwd = 2)

# accuracy comparison at threshold 0.5
log_class <- ifelse(log_probs > 0.5, 1, 0)
svm_class <- ifelse(svm_probs > 0.5, 1, 0)

log_acc <- mean(log_class == as.numeric(as.character(test_set$credit.rating)))
svm_acc <- mean(svm_class == as.numeric(as.character(test_set$credit.rating)))

cat("Logistic Regression Accuracy:", round(log_acc, 4), "\n")
cat("SVM Accuracy:", round(svm_acc, 4), "\n")

table(filtered_data$credit.rating)
table(train_set$credit.rating)
table(test_set$credit.rating)
