# ----------
# Step 0: Install and Load Packages
# ----------
required_packages <- c("this.path", "dplyr", "ggplot2", "lubridate", "geosphere", 
                       "readr", "corrplot", "faraway", "car")

for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ----------
# Step 1: Load and Merge Data
# ----------
setwd(this.path::here())

# Current Directory
cat("Current Directory:", getwd(), "\n")

# Load CSV files
dirty_data <- read_csv("dirty_data.csv")
missing_data <- read_csv("missing_data.csv")
warehouses <- read_csv("warehouses.csv")

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

# Step 3: Calculate Distances to Nearest Warehouse

# 1. Tạo ma trận tọa độ của khách hàng
# - Cột 1: kinh độ (longitude)
# - Cột 2: vĩ độ (latitude)
customer_coords <- cbind(merged_data$customer_long, merged_data$customer_lat)

# 2. Tạo ma trận tọa độ của các kho hàng
# - Cột 1: kinh độ (longitude)
# - Cột 2: vĩ độ (latitude)
warehouse_coords <- cbind(warehouses$lon, warehouses$lat)

# 3. Tính toán ma trận khoảng cách
# - Sử dụng hàm distm từ thư viện geosphere để tính khoảng cách
# - Khoảng cách được tính dựa trên phương pháp Vincenty (độ chính xác cao cho trái đất hình ellipsoid)
# - Ma trận kết quả (all_distances): 
#   + Số hàng: số lượng khách hàng
#   + Số cột: số lượng kho hàng
all_distances <- distm(customer_coords, warehouse_coords, fun = distVincentySphere)
print("-----------Results of All Distance-------")
print(all_distances)
# 4. Tìm khoảng cách nhỏ nhất cho từng khách hàng
# - Sử dụng apply với margin = 1 để áp dụng cho từng hàng (từng khách hàng)
# - Với mỗi hàng, hàm min() tìm giá trị nhỏ nhất trong danh sách khoảng cách đến tất cả các kho
min_distances <- apply(all_distances, 1, min)
print("-----------Min Distance-------")
print(min_distances)
# 5. Tìm chỉ số (index) của kho hàng gần nhất
# - which.min() tìm chỉ số của kho hàng có khoảng cách nhỏ nhất cho từng khách hàng
nearest_warehouse_idx <- apply(all_distances, 1, which.min)
# Idx = 1 -> Nickolson 
# Idx = 2 -> Thompson
# Idx = 3 -> Bakers
# 6. Gán khoảng cách nhỏ nhất vào cột mới
# - Chuyển đổi khoảng cách từ mét sang kilomet (bằng cách chia cho 1000)
# - Làm tròn kết quả đến 4 chữ số sau dấu phẩy
merged_data$distance_to_nearest_warehouse <- round(min_distances / 1000, digits = 4)
print(round)
# 7. Gán tên kho hàng gần nhất vào cột mới
# - Sử dụng chỉ số (index) từ nearest_warehouse_idx để lấy tên kho hàng tương ứng
merged_data$nearest_warehouse <- warehouses$names[nearest_warehouse_idx]

# ----------
# Step 4: Check for Remaining Missing Values
# ----------
na_count <- colSums(is.na(merged_data))
cat("Remaining Missing Values in Each Column:\n")
print(na_count)

# Save the cleaned data if needed
# write_csv(merged_data, "cleaned_data.csv")
