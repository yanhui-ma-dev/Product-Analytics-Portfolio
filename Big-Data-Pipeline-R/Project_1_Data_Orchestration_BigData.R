# Step 1: Read two data files
df_min <- read.csv("final_cleaned_taxi_minimal.csv")
df_time <- read.csv("final_cleaned_with_datetime.csv")

# Step 2: Force conversion to numeric to ensure it is not affected by character formatting.
df_min$latitude <- as.numeric(df_min$latitude)
df_min$longitude <- as.numeric(df_min$longitude)

df_time$latitude <- as.numeric(df_time$latitude)
df_time$longitude <- as.numeric(df_time$longitude)

# Calculate min / max / mean respectively
cat("==== Without datetime conversion ====\n")
cat("Latitude:\n")
cat("  Min:", min(df_min$latitude, na.rm=TRUE), "\n")
cat("  Max:", max(df_min$latitude, na.rm=TRUE), "\n")
cat("  Mean:", mean(df_min$latitude, na.rm=TRUE), "\n")

cat("Longitude:\n")
cat("  Min:", min(df_min$longitude, na.rm=TRUE), "\n")
cat("  Max:", max(df_min$longitude, na.rm=TRUE), "\n")
cat("  Mean:", mean(df_min$longitude, na.rm=TRUE), "\n\n")

cat("==== With datetime conversion ====\n")
cat("Latitude:\n")
cat("  Min:", min(df_time$latitude, na.rm=TRUE), "\n")
cat("  Max:", max(df_time$latitude, na.rm=TRUE), "\n")
cat("  Mean:", mean(df_time$latitude, na.rm=TRUE), "\n")

cat("Longitude:\n")
cat("  Min:", min(df_time$longitude, na.rm=TRUE), "\n")
cat("  Max:", max(df_time$longitude, na.rm=TRUE), "\n")
cat("  Mean:", mean(df_time$longitude, na.rm=TRUE), "\n")
