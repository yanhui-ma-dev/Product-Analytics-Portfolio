# read dataset
# please note that the path of the csv could be changing due to different users, please run the 922_data_clean.ipynb first before runing this R file.
data <- read.csv("~/Desktop/CSIT922/Online_Retail_Cleaned.csv")
# view the dimension of data structure 
dim(data)
head(data)
str(data)

# chech the period of this data
range(data$InvoiceDate)
# overall
summary(data)

# summarised customer characteristic
install.packages("dplyr")
library(dplyr)

# set the last date depend on invoice date 
last_date <- max(data$InvoiceDate)

# summarized customer charateristics
customer_data <- data %>%
  group_by(CustomerID) %>%
  summarise(
    firstPurchaseDate = min(InvoiceDate),
    lastPurchaseDate = max(InvoiceDate),
    frequency = n_distinct(InvoiceNo),
    totalSpent = sum(Quantity * UnitPrice),
    recency = as.numeric(difftime(last_date, lastPurchaseDate, units = "days")),
    productVariety = n_distinct(StockCode)
  ) %>%
  mutate(
    # POSIXct datetime
    firstPurchaseDate = as.POSIXct(firstPurchaseDate, format = "%Y-%m-%d %H:%M:%S"),
    lastPurchaseDate = as.POSIXct(lastPurchaseDate, format = "%Y-%m-%d %H:%M:%S"),
    
    # calculate customer life time
    customer_lifetime = as.numeric(difftime(lastPurchaseDate, firstPurchaseDate, units = "days")),
    
    # calculate average purchase 
    avgPurchaseInterval = ifelse(frequency > 1, customer_lifetime / (frequency - 1), 0)
  )
head(customer_data)
str(customer_data)
dim(customer_data)
summary(customer_data)

#### 1. Clustering with K-means ######
# select factor which is numercial
cluster_data <- customer_data %>%
  select(recency, frequency, totalSpent, avgPurchaseInterval, productVariety, customer_lifetime)

# standarzied data
cluster_data_scaled <- scale(cluster_data)

# apply Eblow
# set function
wss <- function(k) {
  kmeans(cluster_data_scaled, centers = k, nstart = 25)$tot.withinss
}

# try a range of K value
k.values <- 1:10
wss_values <- sapply(k.values, wss)

# analysis the output of Elbow in plot 
plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares (WSS)")

# run K-means
set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

# check distribution of clustering
table(kmeans_result$cluster)

# add cluster to customer data
customer_data$cluster <- kmeans_result$cluster
customer_data$cluster <- as.factor(customer_data$cluster)

# view the centre point
kmeans_result$centers

# visualization with spider 
install.packages("fmsb") 
library(fmsb)

# use cluster centers
centers_df <- as.data.frame(kmeans_result$centers)

# add max/min lines for fmsb requirements
centers_df_radar <- rbind(
  apply(centers_df, 2, max),  
  apply(centers_df, 2, min), 
  centers_df            
)
rownames(centers_df) <- c("Cluster 1 (n=949)", 
                               "Cluster 2 (n=1462)", 
                               "Cluster 3 (n=17)", 
                               "Cluster 4 (n=1910)")

# plot the spider
# color
colors_border <- c("red", "blue", "green", "purple")
colors_fill   <- adjustcolor(colors_border, alpha.f = 0.2)

# spider plot
radarchart(
  centers_df_radar,
  axistype = 1,
  pcol = colors_border,      
  pfcol = colors_fill,      
  plwd = 2,                  
  plty = 1,                 
  cglcol = "grey",   
  cglty = 1,
  axislabcol = "grey",
  vlcex = 0.8            
)

# add labels
legend(
  x = 1.4,
  y = 1.4,
  legend = c("Cluster 1 (n=949)", 
             "Cluster 2 (n=1462)", 
             "Cluster 3 (n=17)", 
             "Cluster 4 (n=1910)"),
  bty = "n",
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 0.9,
  pt.cex = 2
)

# analysis country by customer count per cluster
library(ggplot2)

# prepare unique customer country mapping
customer_country <- data %>%
  select(CustomerID, Country) %>%
  distinct(CustomerID, .keep_all = TRUE) # remove duplicates to ensure that each customer corresponds to only one country

# merge Country into customer_data
customer_data <- customer_data %>%
  left_join(customer_country, by = "CustomerID")

# count the number of customers in each country in each cluster
cluster_country_counts <- customer_data %>%
  group_by(cluster, Country) %>%
  summarise(count = n(), .groups = "drop")

# take the top 3 countries with the most customers in each cluster
top3_per_cluster <- cluster_country_counts %>%
  group_by(cluster) %>%
  slice_max(order_by = count, n = 3, with_ties = FALSE)

# plot with bar
ggplot(top3_per_cluster, aes(x = reorder(Country, -count), y = count, fill = factor(cluster))) +
  geom_col() +
  facet_wrap(~ cluster, scales = "free") +
  labs(
    title = "Top 3 Countries by Customer Count per Cluster",
    x = "Country",
    y = "Customer Count",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

getwd()
# output customer_data
write.csv(customer_data, file = "/Users/amanda/Downloads/customer_data.csv", row.names = FALSE)

### 2. Prediction ###
# set high value customer
quantile(customer_data$totalSpent, probs = c(0.1, 0.2, 0.5, 0.8, 0.9))
customer_data$high_value <- ifelse(customer_data$totalSpent >= 2000, 1, 0)

# select factor
input_data <- customer_data %>%
  select(frequency, recency, productVariety, customer_lifetime,
         avgPurchaseInterval, high_value)

# 3. z-score
library(caret)
pre_proc <- preProcess(input_data[, -6], method = c("center", "scale"))
scaled_data <- predict(pre_proc, input_data[, -6]) # exclude high_value
final_data <- cbind(scaled_data, high_value = input_data$high_value) # high_value as target value

# convert to factor (yes / no)
final_data$high_value <- ifelse(final_data$high_value == 1, "yes", "no") %>% 
  as.factor()

# split training set and testing set
set.seed(123)
train_index <- createDataPartition(final_data$high_value, p = 0.8, list = FALSE)
train_data <- final_data[train_index, ]
test_data  <- final_data[-train_index, ]

# ANN modelling uses cross verification
library(nnet)
ctrl <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

ann_model <- train(
  high_value ~ .,
  data = train_data,
  method = "nnet",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = expand.grid(size = c(3, 5, 7),
                         decay = c(0.01, 0.1)),
  preProcess = c("center", "scale"),
  trace = FALSE
)

# prediction on testing set
pred_probs <- predict(ann_model, test_data[, -which(names(test_data) == "high_value")], type = "prob")[, "yes"]
pred_class <- ifelse(pred_probs > 0.5, "yes", "no") %>%
  factor(levels = c("yes", "no"))

# print the best parameter
ann_model$bestTune

# set true label for test data
actual <- factor(test_data$high_value, levels = c("yes", "no"))

# assessment with confusion matriax
conf_matrix <- confusionMatrix(pred_class, actual, positive = "yes")
print(conf_matrix)

# ROC analysis
library(pROC)

roc_obj <- roc(actual, pred_probs, levels = c("no", "yes"), direction = "<")
plot(roc_obj, col = "blue", legacy.axes = TRUE)
auc(roc_obj)

### which cluster are more likely to be predicted as high_value
table_cluster_highvalue <- table(customer_data$cluster, customer_data$high_value)
# check the percentage of "yes" for each cluster
prop_table <- prop.table(table_cluster_highvalue, margin = 1) 

# print the result
print(table_cluster_highvalue)
print(round(prop_table, 3))

# visualize the ratio
library(ggplot2)
library(scales)

df <- as.data.frame(prop_table)
colnames(df) <- c("Cluster", "HighValue", "Proportion")

# only one colum :HighValue = 1 
df_plot <- subset(df, HighValue == 1)

ggplot(df_plot, aes(x = as.factor(Cluster), y = Proportion, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Proportion * 100, 1), "%")), 
            vjust = -0.5, size = 4.5) +
  ggtitle("Proportion of High Value Customers by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of High Value = yes") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()
