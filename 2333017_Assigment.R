# ----------
# Step 0: Install and Load Packages
# ----------
required_packages <- c("this.path", "dplyr", "ggplot2", "lubridate", "geosphere", 
                       "readr", "corrplot", "faraway", "car", "ggthemes")

# Install and load necessary packages
for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ----------
# Step 1: Load and Merge Data
# ----------
# Set working directory
setwd(this.path::here())

# Print current working directory
cat("Current Directory:", getwd(), "\n")

# Load CSV files
dirty_data <- read_csv("data/dirty_data.csv")
missing_data <- read_csv("data/missing_data.csv")
warehouses <- read_csv("data/warehouses.csv")

# Parse date columns in dirty_data and missing_data
dirty_data$date <- parse_date_time(dirty_data$date, orders = c("mdy", "ymd", "dmy"))
missing_data$date <- parse_date_time(missing_data$date, orders = c("mdy", "ymd", "dmy"))

# Merge dirty_data and missing_data
merged_data <- rbind(dirty_data, missing_data)

# ----------
# Step 2: Clean Data
# ----------
# Convert 'season' to lowercase and fill missing values
merged_data$season <- tolower(merged_data$season)
month_value <- month(merged_data$date)

merged_data <- merged_data %>%
  mutate(season = case_when(
    !is.na(season) ~ season,  # Keep existing values
    month_value %in% c(12, 1, 2) ~ "winter", 
    month_value %in% c(3, 4, 5) ~ "spring", 
    month_value %in% c(6, 7, 8) ~ "summer", 
    TRUE ~ "autumn"
  ))

# Fill missing values in 'is_happy_customer' using median
median_happy_customer <- round(median(merged_data$is_happy_customer, na.rm = TRUE), digits = 0)
merged_data$is_happy_customer[is.na(merged_data$is_happy_customer)] <- median_happy_customer

# Fill missing values in 'customer_lat' and 'customer_long' using mean
mean_customer_lat <- round(mean(merged_data$customer_lat, na.rm = TRUE), digits = 0)
mean_customer_long <- round(mean(merged_data$customer_long, na.rm = TRUE), digits = 0)

merged_data$customer_lat[is.na(merged_data$customer_lat)] <- mean_customer_lat
merged_data$customer_long[is.na(merged_data$customer_long)] <- mean_customer_long

# Clean order data: Fill NA values in 'order_total' and 'order_price'
merged_data <- merged_data %>%
  mutate(
    order_total = ifelse(
      is.na(order_total), 
      order_price * (100 - coupon_discount) / 100 + delivery_charges, 
      order_total
    ),
    order_price = ifelse(
      is.na(order_price), 
      (order_total - delivery_charges) * 100 / (100 - coupon_discount), 
      order_price
    )
  )

# ----------
# Step 3: Distance Calculations
# ----------
# Create customer coordinates matrix
customer_coords <- cbind(merged_data$customer_long, merged_data$customer_lat)
# Create warehouse coordinates matrix
warehouse_coords <- cbind(warehouses$lon, warehouses$lat)

# Calculate distance between customers and warehouses
all_distances <- distm(customer_coords, warehouse_coords, fun = distVincentySphere)

# Find the minimum distance for each customer (in meters)
min_distances <- apply(all_distances, 1, min)  
# Convert minimum distances to kilometers and store in merged_data
merged_data$distance_to_nearest_warehouse <- round(min_distances / 1000, 4)

# ----------
# Step 4: Check for Remaining Missing Values
# ----------
# Count remaining missing values in each column
na_count <- colSums(is.na(merged_data))
cat("Remaining Missing Values in Each Column:\n")
print(na_count)

# ----------
# Step 5: Outlier Detection Using Boxplots
# ----------
# Boxplot for Distance to Nearest Warehouse
ggplot(data = merged_data, aes(y = distance_to_nearest_warehouse)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Outlier of Distance to Nearest Warehouse",
    y = "Distance to Nearest Warehouse"
  )

# Boxplot for Delivery Charges
ggplot(data = merged_data, aes(y = delivery_charges)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Outlier of Delivery Charge",
    y = "Delivery Charges"
  )

# Boxplot for Order Price
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Outlier of Order Price",
    y = "Order Price"
  )

# Boxplot for Coupon Discount
ggplot(data = merged_data, aes(y = coupon_discount)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Outlier of Coupon Discount",
    y = "Coupon Discount"
  )

# Boxplot for Order Total
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Outlier of Order Total",
    y = "Order Total"
  )

# ----------
# Step 6: Remove Outliers Using IQR Method for distance_to_nearest_warehouse in merged_data
# ----------
# Define IQR for distance_to_nearest_warehouse
# Trong R , hàm quanlite trả về 5 giá trị 
# [1]: 0%
# [2]: 25% [Q1]
# [3]: 50%
# [4]: 75% [Q3]
# [5]: 100%

## Lấy thông tin số các cột cần tính
# Define IQR for each variable
quantiles_dist <- quantile(merged_data$distance_to_nearest_warehouse)
quantiles_deliv <- quantile(merged_data$delivery_charge)
quantiles_price <- quantile(merged_data$order_price)
quantiles_coupon <- quantile(merged_data$coupon_discount)
quantiles_total <- quantile(merged_data$order_total)

# Extract Q1 and Q3
q1_dist <- quantiles_dist[2]
q3_dist <- quantiles_dist[4]
q1_deliv <- quantiles_deliv[2]
q3_deliv <- quantiles_deliv[4]
q1_price <- quantiles_price[2]
q3_price <- quantiles_price[4]
q1_coupon <- quantiles_coupon[2]
q3_coupon <- quantiles_coupon[4]
q1_total <- quantiles_total[2]
q3_total <- quantiles_total[4]

# Calculate IQR for each variable
IQR_dist <- q3_dist - q1_dist
IQR_deliv <- q3_deliv - q1_deliv
IQR_price <- q3_price - q1_price
IQR_total <- q3_total - q1_total
IQR_coupon <- q3_coupon - q1_coupon

# Calculate lower and upper quantiles
calc_lower <- function(Q1, IQR) { return(Q1 - 1.5 * IQR) }
calc_upper <- function(Q3, IQR) { return(Q3 + 1.5 * IQR) }

# Lower and upper quantiles for each variable
lower_dist <- calc_lower(q1_dist, IQR_dist)
upper_dist <- calc_upper(q3_dist, IQR_dist)

lower_deliv <- calc_lower(q1_deliv, IQR_deliv)
upper_deliv <- calc_upper(q3_deliv, IQR_deliv)

lower_price <- calc_lower(q1_price, IQR_price)
upper_price <- calc_upper(q3_price, IQR_price)

lower_total <- calc_lower(q1_total, IQR_total)
upper_total <- calc_upper(q3_total, IQR_total)

lower_coupon <- calc_lower(q1_coupon, IQR_coupon)
upper_coupon <- calc_upper(q3_coupon, IQR_coupon)

cat(lower_dist)
cat(upper_dist)