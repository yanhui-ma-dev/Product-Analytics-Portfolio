# read dataset
data <- read.csv("creditworthiness.csv")
# filter customers who have been rating
filtered_data <- data[data$credit.rating !=0, ]
# view the dimension of data structure in filtered data
dim(filtered_data)
### split dataset into 50% training set and 50% test set based on filtered dataset
# assign variable n
n <- nrow(filtered_data)
# random sample 50% as train index
train_index <- sample(1:n, size = floor(0.5*n))
# set training and test set
train_set <- filtered_data[train_index, ]
test_set <- filtered_data[-train_index, ]
# view the dimension of data structure
dim(train_set)
dim(test_set)
