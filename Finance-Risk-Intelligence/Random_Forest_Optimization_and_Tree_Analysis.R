# Q2.a
# install packages for decision tree
install.packages("tree")
install.packages("rpart.plot")
# loading resources
library(tree)
library(rpart)
library(randomForest)
library(rpart.plot)

# create a decision tree with rpart
tree_model = rpart(credit.rating~., data = train_set, method = "class")
# view the result of this decision tree
print(tree_model)
# plot the tree
rpart.plot(tree_model,
           main = "Decision Tree for Credit Rating",
           type = 3,
           extra = 101
)

# Q2.b
# create median customer
median_customer <- data.frame(
  functionary = 0,
  re.balanced..paid.back..a.recently.overdrawn.current.acount = 1,
  FI3O.credit.score = 1,
  gender = 0,
  X0..accounts.at.other.banks = 3,
  credit.refused.in.past. = 0,
  years.employed = 3,
  savings.on.other.accounts = 3,
  self.employed. = 0,
  max..account.balance.12.months.ago = 3,
  min..account.balance.12.months.ago = 3,
  avrg..account.balance.12.months.ago = 3,
  max..account.balance.11.months.ago = 3,
  min..account.balance.11.months.ago = 3,
  avrg..account.balance.11.months.ago = 3,
  max..account.balance.10.months.ago = 3,
  min..account.balance.10.months.ago = 3,
  avrg..account.balance.10.months.ago = 3,
  max..account.balance.9.months.ago = 3,
  min..account.balance.9.months.ago = 3,
  avrg..account.balance.9.months.ago = 3,
  max..account.balance.8.months.ago = 3,
  min..account.balance.8.months.ago = 3,
  avrg..account.balance.8.months.ago = 3,
  max..account.balance.7.months.ago = 3,
  min..account.balance.7.months.ago = 3,
  avrg..account.balance.7.months.ago = 3,
  max..account.balance.6.months.ago = 3,
  min..account.balance.6.months.ago = 3,
  avrg..account.balance.6.months.ago = 3,
  max..account.balance.5.months.ago = 3,
  min..account.balance.5.months.ago = 3,
  avrg..account.balance.5.months.ago = 3,
  max..account.balance.4.months.ago = 3,
  min..account.balance.4.months.ago = 3,
  avrg..account.balance.4.months.ago = 3,
  max..account.balance.3.months.ago = 3,
  min..account.balance.3.months.ago = 3,
  avrg..account.balance.3.months.ago = 3,
  max..account.balance.2.months.ago = 3,
  min..account.balance.2.months.ago = 3,
  avrg..account.balance.2.months.ago = 3,
  max..account.balance.1.months.ago = 3,
  min..account.balance.1.months.ago = 3,
  avrg..account.balance.1.months.ago = 3
)

# use tree_model to predict
predicted_rating <- predict(tree_model, newdata = median_customer, type = "class")
# print predicted result
cat("Predicted result for the median customer is:", as.character(predicted_rating), "\n")


# Q2.c
# predict the credit rating for the test set
predict_result <- predict(tree_model, newdata = test_set, type = "class")
# create the confusion matrix
confusion_matrix <- table(test_set$credit.rating, predict_result)
# print the confusion matrix
print(confusion_matrix)
# calculate the overall accuracy rate
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# print the result of the overall accuracy
cat(accuracy)

# Q2.d
# define class probabilities
p1 <- 0.23853211
p2 <- 0.50254842
p3 <- 0.25891947
# calculate the entropy H(D) of the root node
H_root <- - (p1 * log2(p1) + p2 * log2(p2) + p3 * log2(p3))
H_root
## Entropy gain after the first split at the top of the tree:
# Step 1: Left child node
# set class probabilities
p1_left <- 0.49346405
p2_left <- 0.32679739
p3_left <- 0.17973856
# calculate the entropy H(D) of left
H_left <- - (p1_left * log2(p1_left) + p2_left * log2(p2_left) + p3_left * log2(p3_left))
cat(H_left)

# define class probabilities
p1 <- 0.23853211
p2 <- 0.50254842
p3 <- 0.25891947
# calculate the entropy H(D) of the root node
H_root <- - (p1 * log2(p1) + p2 * log2(p2) + p3 * log2(p3))
H_root

## Entropy gain after the first split at the top of the tree:
# Step 1: Left child node
# set class probabilities
p1_left <- 0.49346405
p2_left <- 0.32679739
p3_left <- 0.17973856
# calculate the entropy H(D) of left
H_left <- - (p1_left * log2(p1_left) + p2_left * log2(p2_left) + p3_left * log2(p3_left))
cat(H_left)

# Step 2: Right child node
p1_right <- 0.12296296
p2_right <- 0.58222222
p3_right <- 0.29481481
# calculate the entropy H(D) of right
H_right <- - (p1_right * log2(p1_right) + p2_right * log2(p2_right) + p3_right * log2(p3_right))
cat(H_right)

# Step 3: Calculate weighted average entropy
total_samples = 981
left_sample <- 306
rigtt_sample <- 675
weighted_entropy <- (left_sample/total_samples) * H_left + (rigtt_sample/total_samples) * H_right
cat(weighted_entropy)

# calculate information gain
Infor_gain = H_root - weighted_entropy
cat(Infor_gain)

# Q2.e
# make sure CR as factor
train_set$credit.rating <- as.factor(train_set$credit.rating)
#create the random forest model
rf_model <- randomForest(credit.rating ~., data = train_set)
#print the model summary
print(rf_model)
# generate confusion matrix from the model object
cm <-rf_model$confusion
# calculate training accuracy from confusion matrix
total_correct <- sum(diag(cm))
total_instances <- sum(cm[, "class.error"]*rowSums(cm)) + total_correct
accuracy <- total_correct/total_instances
# print the training accuracy
print(paste("Training Accuracy Rate: ", round(accuracy,4)))

# test based on test set
test_pred <- predict(rf_model, newdata = test_set)
# calculate acc on test set
test_acc <- mean(test_pred == test_set$credit.rating)
# print the test acc rate
cat("Test Accuracy Rate:", round(test_acc, 4), "\n")

### to optimized
# set exploring space
ntree_vals <- c(500, 700, 900, 1000)
mtry_vals <- seq(6, 20, 2)
# create a table to record
results <- data.frame(ntree = integer(), mtry =integer(), train_acc = numeric(), oob_error=numeric())
# exploring grid
for (nt in ntree_vals) {
  for (mt in mtry_vals) {
    cat("Training RF with ntree =", nt, ", mtry =", mt, "\n")
    
    rf_model <- randomForest(
      credit.rating ~ ., data = train_set,
      ntree = nt, mtry = mt, importance = FALSE
    )
    # TEST ACC based on train set
    cm <- rf_model$confusion
    train_acc <- sum(diag(cm)) / sum(cm[, 1:3])
    # OOB error at final tree
    oob <- rf_model$err.rate[nt, "OOB"]
    
    # inserting results
    results <- rbind(results,
                     data.frame(ntree = nt, mtry = mt,
                                train_acc =  train_acc, oob_error = oob))
  }
}
# print results
print(results)

# Q2.f
# set SEED TO ENSURE RESULT CANBE REPEAT
set.seed(123)
# create RF model
rf_model <- randomForest(credit.rating ~., data = train_set, ntree = 900, mtry = 18)
# print the model summary
print(rf_model)
# extract cm from the model
cm <- rf_model$confusion
# test based on test set
test_pred <- predict(rf_model, newdata = test_set)
#create cm for the test set
rf_test_cm <- table(test_set$credit.rating, test_pred)
# print the cm for test set
print(rf_test_cm)
# calculate the overall acc for the teat set
rf_acc_test <- sum(diag(rf_test_cm) / sum(rf_test_cm))
# print the overall acc of the test set
print(paste("Overall test accuracy: ", round(rf_acc_test, 4)))
