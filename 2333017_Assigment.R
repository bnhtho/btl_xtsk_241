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
d1 = quantile(merged_data$order_price)
print(d1)
