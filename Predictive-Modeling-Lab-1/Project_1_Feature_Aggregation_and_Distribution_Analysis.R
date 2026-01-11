# Load the cleaned dataset
df <- read.csv("final_cleaned_with_datetime.csv")

# Verify the raw datetime column format
df$datetime

# Install and load lubridate for efficient date-time manipulation
# install.packages("lubridate")
library(lubridate)

# Task: Standardize datetime strings to POSIXct objects
# Parsing the datetime column using UTC to ensure cross-regional consistency
df$datetime_parsed <- ymd_hms(df$datetime, tz = "UTC")

# Alternative parsing for mixed formats if ymd_hms fails
# df$datetime_parsed <- parse_date_time(df$datetime, orders = "ymd HMS", tz = "UTC")

# Verify data type and structure of the parsed datetime
str(df$datetime_parsed)

# Data Quality Check: Verify the number of NA values generated during parsing
# Aiming for 0 or negligible missing values to maintain data integrity
sum(is.na(df$datetime_parsed))

# Export the standardized dataset for downstream analysis
write.csv(df, "final_cleaned_with_utc.csv", row.names = FALSE)

# BUSINESS LOGIC: Calculate active duration per driver (taxi_id)
# Aggregating data to compute the time span (Max - Min) for each unique ID
activity <- aggregate(df$datetime_parsed, by = list(df$taxi_id), 
                      FUN = function(x) difftime(max(x), min(x), units = "hours"))

# Rename columns for clarity in reporting
colnames(activity) <- c("taxi_id", "active_hours")

# METRIC EXTRACTION: Identify outliers and central tendency in driver activity
most_active <- activity[which.max(activity$active_hours), ]
least_active <- activity[which.min(activity$active_hours), ]
average_hours <- mean(as.numeric(activity$active_hours))

# OUTPUT: Summary statistics for Product/Operations reporting
cat("==== Task 1(c) - Analysis Results ====\n")
cat("Most active driver: ", most_active$taxi_id, " (", round(as.numeric(most_active$active_hours), 2), " hours)\n")
cat("Least active driver:", least_active$taxi_id, " (", round(as.numeric(least_active$active_hours), 2), " hours)\n")
cat("Average active time:", round(average_hours, 2), " hours\n")