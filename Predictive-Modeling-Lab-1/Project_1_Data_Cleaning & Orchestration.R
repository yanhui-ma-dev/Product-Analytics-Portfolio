# install packages
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")

# loading them
library(data.table)
library(dplyr)
library(ggplot2)

# read data
taxi_data <- fread("taxi.csv", header = TRUE)

# check data format
head(taxi_data)
str(taxi_data)
names(taxi_data)

# check NA values
invalid_na <- taxi_data %>%
  filter(is.na(Latitude) | is.na(Longitude))

# print result
cat("NA points found:", nrow(invalid_na), "\n")

# check 0,0
invalid_zero <- taxi_data %>%
  filter(Latitude == 0 & Longitude == 0)

cat("(0,0) points found:", nrow(invalid_zero), "\n")

# change datetime format
taxi_data$datetime <- as.POSIXct(taxi_data$`Date and Time`, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

# check repeat points for the same taxi in countinue time
# view the first columns
names(taxi_data)[1:5] 
# record the total amount for original
total_before <- nrow(taxi_data)
# check and remove completely repeat records
taxi_data_dedup <- taxi_data[!duplicated(taxi_data[, 1:3]), ]

# print result
total_after <- nrow(taxi_data_dedup)
duplicates_removed <- total_before - total_after

cat("Total repeated rows removed (based on first 3 columns):", duplicates_removed, "\n")
cat("Remaining rows after deduplication:", total_after, "\n")

### amount of invalid is 0

### Conducting Outliers
# set Rome boundaries
lat_min <- 41.65
lat_max <- 42.05
lon_min <- 12.20
lon_max <- 12.73

# mark outliers
taxi_data$outliers <- with(taxi_data,
                              Latitude < lat_min | Latitude > lat_max |
                                Longitude < lon_min | Longitude > lon_max
)

table(taxi_data$outliers)

### Noise = Influence points that interfere with the analysis of major driving behaviors
# calcuate speed with taxi data
install.packages("geosphere") 
library(geosphere) 

# ranking data
taxi_data <- taxi_data %>%
  arrange(DriveNo, datetime)

# calculate ditance and time difference
taxi_data <- taxi_data %>%
  group_by(DriveNo) %>%
  mutate(
    prev_lat = lag(Latitude),
    prev_lon = lag(Longitude),
    prev_time = lag(datetime),
    dist_m = distHaversine(
      cbind(Longitude, Latitude),
      cbind(prev_lon, prev_lat)
    ),
    time_diff = as.numeric(difftime(datetime, prev_time, units = "secs")),
    speed_mps = ifelse(!is.na(dist_m) & time_diff > 0, dist_m / time_diff, NA),
    speed_kmph = speed_mps * 3.6
  ) %>%
  ungroup()
# view distribution of total speed
summary(taxi_data$speed_kmph)

# using quantile
quantile(taxi_data$speed_kmph, probs = c(0.5, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)

# calculate speed + mark jump point noise
taxi_data <- taxi_data %>%
  group_by(DriveNo) %>%
  mutate(
    prev_lat = lag(Latitude),
    prev_lon = lag(Longitude),
    prev_time = lag(datetime),
    dist_m = distHaversine(
      cbind(Longitude, Latitude),
      cbind(prev_lon, prev_lat)
    ),
    time_diff = as.numeric(difftime(datetime, prev_time, units = "secs")),
    speed_mps = ifelse(!is.na(dist_m) & time_diff > 0, dist_m / time_diff, NA),
    speed_kmph = speed_mps * 3.6,
    noise_jump = ifelse(speed_kmph > 200, TRUE, FALSE)
  ) %>%
  ungroup()

# print result
cat("Speed-based noise points (>200 km/h):", sum(taxi_data$noise_jump, na.rm = TRUE), "\n")

# 2Dplot
taxi_data <- taxi_data %>%
  mutate(point_type = case_when(
    is.na(Latitude) | is.na(Longitude) | (Latitude == 0 & Longitude == 0) ~ "Invalid",
    outliers == TRUE ~ "Outlier",
    noise_jump == TRUE ~ "Noise",
    TRUE ~ "Normal"
  ))

# set color
point_colors <- c(
  "Invalid" = "#FF0000",
  "Outlier" = "#6a3d9a",
  "Noise"   = "#ff7f00",
  "Normal"  = "#A6CEE3"
)

# sample 200,000 
taxi_sample <- taxi_data %>% sample_n(200000)
# plot with all value
taxi_sample <- taxi_data

# drawing in full value
ggplot(taxi_data, aes(x = Longitude, y = Latitude, color = point_type)) +
  geom_point(size = 0.08, alpha = 0.3) +
  scale_color_manual(values = point_colors) +
  coord_fixed(xlim = c(11.8, 13.5), ylim = c(41.2, 42.3)) + 
  labs(
    title = "Rome Taxi GPS Points",
    color = "Point Type",
    x = "Longitude",
    y = "Latitude"
  )  +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

# removed invalid outliers noise
taxi_cleaned <- taxi_data %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude),
    !(Latitude == 0 & Longitude == 0),
    outliers != TRUE,
    noise_jump != TRUE
  )
total_raw <- nrow(taxi_data)

table(taxi_data$point_type)
# print summary
cat("otal cleaned GPS points:", nrow(taxi_cleaned), "\n")
cat("Cleaned ratio:", round(nrow(taxi_cleaned) / nrow(taxi_data) * 100, 2), "%\n")

# Plot of Cleaned Data
ggplot(taxi_cleaned, aes(x = Longitude, y = Latitude)) +
  geom_point(color = "skyblue", size = 0.03, alpha = 0.5) +
  coord_fixed() +
  labs(
    title = "Rome Taxi Cleaned GPS Points",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2)
  )

# b
# calculate minimum, maximum and mean value
summary_locations <- taxi_cleaned %>%
  summarise(
    min_latitude = min(Latitude, na.rm = TRUE),
    max_latitude = max(Latitude, na.rm = TRUE),
    mean_latitude = mean(Latitude, na.rm = TRUE),
    min_longitude = min(Longitude, na.rm = TRUE),
    max_longitude = max(Longitude, na.rm = TRUE),
    mean_longitude = mean(Longitude, na.rm = TRUE)
  )

print(summary_locations)

# calculate activity
# ensure work on cleaned data
taxi_activity <- taxi_cleaned %>%
  group_by(DriveNo) %>%
  summarise(
    start_time = min(datetime),
    end_time = max(datetime),
    duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
    .groups = "drop"
  )

# most activity
most_active <- taxi_activity %>% filter(duration_hours == max(duration_hours))
# least avtivity
least_active <- taxi_activity %>% filter(duration_hours == min(duration_hours))
# avergae
average_active <- mean(taxi_activity$duration_hours, na.rm = TRUE)

# print results
cat("Most Active Taxi:\n")
print(most_active)

cat("Least Active Taxi:\n")
print(least_active)

cat("Average Operating Duration (hours):", round(average_active, 2), "\n")

# d
# 1) plot the location points for 261
# filtering data for taxi 261
taxi_target <- taxi_cleaned %>% filter(DriveNo == 261)

# drawing
ggplot(taxi_target, aes(x = Longitude, y = Latitude)) +
  geom_point(color = "#A6CEE3", size = 0.15, alpha = 0.4) +
  labs(
    title = "Location Points for Taxi ID 261",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_fixed() +
  theme_minimal()

# 2) compare the mean, min, and max location value of taxi=ID with the global mean, min, and max.
# calculate results for taxi 261
location_stats_261 <- taxi_cleaned %>%
  filter(DriveNo == 261) %>%
  summarise(
    min_lat = min(Latitude),
    max_lat = max(Latitude),
    mean_lat = mean(Latitude),
    min_lon = min(Longitude),
    max_lon = max(Longitude),
    mean_lon = mean(Longitude)
  )

# calculate global location
location_stats_global <- taxi_cleaned %>%
  summarise(
    min_lat = min(Latitude),
    max_lat = max(Latitude),
    mean_lat = mean(Latitude),
    min_lon = min(Longitude),
    max_lon = max(Longitude),
    mean_lon = mean(Longitude)
  )

# display results
cat("Taxi 261 Location Stats:\n")
print(location_stats_261)

cat("Global Location Stats:\n")
print(location_stats_global)

# 3) compare activity
# calculate 261
duration_261 <- taxi_cleaned %>%
  filter(DriveNo == 261) %>%
  summarise(
    start_time = min(datetime),
    end_time = max(datetime),
    duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
  )

# calculate global
duration_all <- taxi_cleaned %>%
  group_by(DriveNo) %>%
  summarise(
    duration = as.numeric(difftime(max(datetime), min(datetime), units = "hours"))
  ) %>%
  summarise(
    min_duration = min(duration),
    max_duration = max(duration),
    mean_duration = mean(duration)
  )

# print results
cat("Taxi 261 duration (hours):", round(duration_261$duration_hours, 2), "\n")
print(duration_all)

# 4) compute the distance of 261 driving
# set earth radiu
R <- 6371000

# extract taxi 261 and ranking as datetime
taxi_261 <- taxi_cleaned %>%
  filter(DriveNo == 261) %>%
  arrange(datetime)


# prepare the forward and backward point pairs (i and i+1)
lat1 <- head(taxi_261$Latitude, -1)
lat2 <- tail(taxi_261$Latitude, -1)
lon1 <- head(taxi_261$Longitude, -1)
lon2 <- tail(taxi_261$Longitude, -1)

# convert to radians
deg2rad <- function(deg) { deg * pi / 180 }

lat1_rad <- deg2rad(lat1)
lat2_rad <- deg2rad(lat2)
dlon_rad <- deg2rad(lon2 - lon1)
dlat_rad <- deg2rad(lat2 - lat1)

# Haversine compute
a <- (sin(dlat_rad / 2))^2 + cos(lat1_rad) * cos(lat2_rad) * (sin(dlon_rad / 2))^2
c <- 2 * atan2(sqrt(a), sqrt(1 - a))
distance_m <- R * c

# total distance (meters & kilometers)
total_distance_m <- sum(distance_m, na.rm = TRUE)
total_distance_km <- round(total_distance_m / 1000, 2)

# print result
cat("Total distance traveled by taxi 261:", total_distance_km, "km\n")

cat("Total distance traveled by taxi 261:", total_distance_km, "km\n")
