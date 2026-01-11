# Load necessary libraries
install.packages("randomForest")
library(randomForest)

# Use the already loaded data
trainValues <- knownData[,1:45]
trainTargets <- as.factor(knownData[,46]) 

# Data Splitting: 70% Training, 30% Testing for model validation
set.seed(8090)
split_index <- sample(1:nrow(trainValues), size = 0.7 * nrow(trainValues))
inputsTrain <- trainValues[split_index, ]
inputsTest  <- trainValues[-split_index, ]
targetsTrain <- trainTargets[split_index]
targetsTest  <- trainTargets[-split_index]

# Train Random Forest model with 700 trees
# importance = TRUE enables feature importance analysis later
rf_model <- randomForest(
  x = inputsTrain, 
  y = targetsTrain,
  ntree = 700,    
  mtry = 10,             
  importance = TRUE    
)

rf_preds <- predict(rf_model, newdata = inputsTest, type = "response")

conf_matrix <- table(True = targetsTest, Predicted = rf_preds)
print(conf_matrix)

# Calculate accuracy
acc <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Random Forest Accuracy (no 402040):", round(acc * 100, 2), "%\n")

rf_probs <- predict(rf_model, newdata = inputsTest, type = "prob")

# Confidence-based Reject Option Logic (The "402040" Strategy)
# This function filters out ambiguous predictions where the max probability is low
encode402040 <- function(prob_mat, l=0.5, h=0.5) {
  apply(prob_mat, 1, function(x) {
    if (max(x) < l || max(x) > h) {
      return(which.max(x))
    } else {
      return(0) 
    }
  })
}

predicted_402040 <- encode402040(rf_probs, l = 0.5, h = 0.5)
conf402040 <- table(True = targetsTest, Predicted = predicted_402040)
print(conf402040)

# Calculate accuracy & coverage
correct <- sum(diag(conf402040[, colnames(conf402040) != "0"]))
total_predicted <- sum(conf402040[, colnames(conf402040) != "0"])
accuracy <- correct / total_predicted
coverage <- total_predicted / sum(conf402040)

cat("Random Forest Accuracy (402040):", round(accuracy * 100, 2), "%\n")
cat("Coverage (402040):", round(coverage * 100, 2), "%\n")

importance(rf_model)
varImpPlot(rf_model)
varImpPlot(rf_model, type=1)  
