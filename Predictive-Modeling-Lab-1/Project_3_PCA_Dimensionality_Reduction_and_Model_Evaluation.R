# remove variables and memory
rm(list = ls())
gc()
# check Broom 

# loading resources
install.packages("RSNNS")
library(RSNNS)

# read data
fullDataSet <- read.csv("creditworthiness.csv")
knownData <- subset(fullDataSet, fullDataSet[,46] > 0)

# view structure of data
str(knownData)

# spliting trainset and testset
trainValues <- knownData[,1:45]
trainTargets <- decodeClassLabels(knownData[,46])
trainset <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.3)
trainset <- normTrainingAndTestSet(trainset)

# using PCA view trainSet，observe whether there are obvious "outliers" or overlaps between classes 
pca <- prcomp(trainset$inputsTrain, scale. = TRUE)
target_labels <- apply(trainset$targetsTrain, 1, function(x) which(x == 1))

# view distribution of different classes
label_distribution <- table(target_labels)
print(label_distribution)

# set colors for different classes
palette(c("red", "blue", "darkgreen"))
# plot
plot(
  pca$x[,1], pca$x[,2],
  col = target_labels,
  pch = 16, 
  main = "PCA Visualization of Normalized TrainSet",
  xlab = "PC1", ylab = "PC2"
)

# retrieve scope of xlab and ylab
usr <- par("usr")
# set the bottom of legend in middle
x_center <- mean(usr[1:2])
y_bottom <- usr[3] - 0.8

# add legend in customer location
legend(
  x = x_center, y = y_bottom, 
  legend = c("Class 1", "Class 2", "Class 3"), 
  col = 1:3, 
  pch = 16,
  horiz = TRUE,
  xpd = TRUE,
  bty = "n",
  cex = 0.9,
  xjust = 0.5
  )
# identify outliers by calculating the distance between each sample and the center of clusters
mahal_dist <- mahalanobis(pca$x[,1:2], colMeans(pca$x[,1:2]), cov(pca$x[,1:2]))
outliers <- which(mahal_dist > quantile(mahal_dist, 0.99))
outlier_labels <- apply(trainset$targetsTrain[outliers, ], 1, function(x) which(x == 1))

# print distribution of outliers
cat("total outliers =", length(outliers), "\n")
print(table(outlier_labels))

# show the top few outlier examples
head(data.frame(
  Index = outliers,
  Distance = mahal_dist[outliers],
  Label = outlier_labels
), 15)

# create boolean vector to lable non-outlier samples
is_outlier <- rep(FALSE, nrow(trainset$inputsTrain))
is_outlier[outliers] <- TRUE

# filtering trainset
filtered_inputs <- trainset$inputsTrain[!is_outlier, ]
filtered_targets <- trainset$targetsTrain[!is_outlier, ]

# train MLP model
model <- mlp(
  trainset$inputsTrain,
  trainset$targetsTrain,
  size= c(15,10), 
  learnFuncParams=c(0.005),
  maxit=300, 
  inputsTest=trainset$inputsTest, 
  targetsTest=trainset$targetsTest
  )
# assess model and pridicting
predictTestSet <- predict(model,trainset$inputsTest)
confusionMatrix(trainset$targetsTrain,fitted.values(model))
# testing based on TestSet
confusionMatrix(trainset$targetsTest,predictTestSet)


par(mfrow=c(2,2))
plotIterativeError(model)
plotRegressionError(predictTestSet[,2], trainset$targetsTest[,2])
plotROC(fitted.values(model)[,2], trainset$targetsTrain[,2])
plotROC(predictTestSet[,2], trainset$targetsTest[,2])

# resET CANVAS
par(mfrow = c(1,1))

#confusion matrix with 402040-method based on TrainSet
confusionMatrix(trainset$targetsTrain, encodeClassLabels(fitted.values(model),method="402040", l=0.5, h=0.5))

# 402040-method test based on TestSet
predicted_402040 <- encodeClassLabels(predictTestSet, method="402040", l=0.5, h=0.5)
confusionMatrix(trainset$targetsTest, predicted_402040)

#show detailed information of the model
summary(model)
model
weightMatrix(model)
extractNetInfo(model)

