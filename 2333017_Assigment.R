# ----------
# 1. Chuẩn bị dữ liệu
# ----------
# Cập nhật ngày 3/12/2024: Thêm bảng gt
required_packages <- c("this.path", "dplyr", "ggplot2", "lubridate", "geosphere", 
                       "readr", "corrplot", "faraway", "car", "ggthemes","gt","nortest","knitr","FSA")

# Install and load necessary packages
for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
# ----------
# Step 1: Cài đặt thư viện và gộp dữ liệu
# ----------
# Chỉnh đường dẫn của thư mục gốc
setwd(this.path::here())
# Load dữ liệu mẫu 
dirty_data <- read_csv("data/dirty_data.csv")
missing_data <- read_csv("data/missing_data.csv")
warehouses <- read_csv("data/warehouses.csv")

# Thay đổi định dạng ngày tháng năm đồng bộ
dirty_data$date <- parse_date_time(dirty_data$date, orders = c("mdy", "ymd", "dmy"))
missing_data$date <- parse_date_time(missing_data$date, orders = c("mdy", "ymd", "dmy"))

# Gộp dữ liệu dirty_data và missing_data
merged_data <- rbind(dirty_data, missing_data)
# ----------
# 1. Làm sạch
# ----------

# 1.0 Hiện thị dữ liệu bị khuyết(NA)
na_cout<- colSums(is.na(merged_data ))
print(na_cout)

# 1.1 Order_Total và Order_Price
merged_data <- merged_data %>%
  mutate(
    # Điền giá trị NULL cho order_total
    order_total = ifelse(
      is.na(order_total), 
      order_price * (100 - coupon_discount) / 100 + delivery_charges, 
      order_total
    ),
    # Điền giá trị NULL cho order_price
    order_price = ifelse(
      is.na(order_price), 
      (order_total - delivery_charges) * 100 / (100 - coupon_discount), 
      order_price
    )
  )

# 1.2 Mùa
season_unique_before<-unique(merged_data$season)
print(season_unique_before)

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

## After
season_unique_after<-unique(merged_data$season)
print(season_unique_after)

# 1.3 Độ hài lòng của khách hàng
median_happy_customer <- round(median(merged_data$is_happy_customer, na.rm = TRUE), digits = 0)
merged_data$is_happy_customer[is.na(merged_data$is_happy_customer)] <- median_happy_customer

# ----------
# 2. Mô Tả Thống Kê
# ----------
# ----------
# 2.1: Thể hiện giá trị ngoại lai
# ----------

# Boxplot for Order Price
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Outlier of Order Price",
    y = "Order Price"
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
# 2.2: Loại bỏ Outlier (điểm ngoại lai)
# ----------
# Trong R , hàm quanlite trả về 5 giá trị 
# [1]: 0%
# [2]: 25% [Q1]
# [3]: 50%
# [4]: 75% [Q3]
# [5]: 100%
# Hàm tứ phân vị trong R
quantiles_price <- quantile(merged_data$order_price)
quantiles_total <- quantile(merged_data$order_total)

# Lấy giá trị Q1 và Q2

q1_price <- quantiles_price[2]
q3_price <- quantiles_price[4]
q1_total <- quantiles_total[2]
q3_total <- quantiles_total[4]

# Tính tứ phân vị
IQR_price <- q3_price - q1_price
IQR_total <- q3_total - q1_total
# Hàm Tính giới hạn trên và dưới
calc_lower <- function(Q1, IQR) { return(Q1 - 1.5 * IQR) }
calc_upper <- function(Q3, IQR) { return(Q3 + 1.5 * IQR) }

# Gọi hàm để tính giá trị giới hạn trên và dưới cho hai cột
lower_price <- calc_lower(q1_price, IQR_price)
upper_price <- calc_upper(q3_price, IQR_price)
lower_total <- calc_lower(q1_total, IQR_total)
upper_total <- calc_upper(q3_total, IQR_total)

## Thay thế các giá trị outlier bằng giá trị giới hạn trên và dưới.
for (i in 1:length(merged_data$order_total)) {
  if (merged_data$order_total[i] > upper_total) {
    merged_data$order_total[i] = upper_total
  } else if(merged_data$order_total[i] < lower_total ){
    merged_data$order_total[i] = lower_total
    
  }
}

for (i in 1:length(merged_data$order_price)) {
  if (merged_data$order_price[i] > upper_price) {
    merged_data$order_price[i] = upper_price
  } else if(merged_data$order_price[i] < lower_price){
    merged_data$order_price[i] = lower_price
  }
}

# ----------
# 2.3: Vẽ lại đồ thị sau khi loại bỏ Outlier
# ----------
# Boxplot for Order Price
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Đồ thị thể hiện tứ phân vị của cột Order Price",
    y = "Order Price"
  )


# Boxplot for Order Total
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Đồ thị thể hiện tứ phân vị của cột Order Total",
    y = "Order Total"
  )
# ----------
# 2.4: Vẽ đồ thị
# ----------

# Tóm tắt chỉ số theo mùa
season_summary <- merged_data %>%
  group_by(season) %>%
  summarise(
    # Khảo sát xem trung bình 
    avg_delivery_charge = mean(delivery_charges, na.rm = TRUE),
    total_orders = n(),
    avg_order_total = mean(order_total, na.rm = TRUE),
    ## Tổng chi phí giao hàng qua các mùa
    total_orders_with_discount = sum(coupon_discount > 0, na.rm = TRUE),  # Tổng số đơn hàng có giảm giá[3]
    total_delivery_charges = sum(delivery_charges, na.rm = TRUE)
    
  )
print(season_summary)

# Đồ thị thể hiện tổng số lượng đơn hàng theo mùa
ggplot(data = season_summary, aes(x = season, y = total_orders, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_orders), vjust = 2,color = "white", ) +  # Thêm nhãn số liệu
  theme_minimal() +
  labs(
    title = "Tổng số đơn hàng trong từng mùa",
    x = " ",
    y = "Số đơn hàng"
  ) +
  scale_fill_brewer(palette = "Paired")

# Đồ thị tổng chi phí đơn hàng theo từng mùa
ggplot(data = season_summary, aes(x = season, y = total_delivery_charges, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_delivery_charges), vjust = 2,color = "white", ) +  # Hiển thị số liệu trên đầu thanh
  theme_minimal() +
  labs(
    title = "Tổng phí giao hàng từng mùa",
    x = " ",
    y = "Phí giao hàng"
  ) +
  scale_fill_brewer(palette = "Set2")

# Số đơn hàng đã áp dụng mã khuyến mãi theo từng mùa
ggplot(data = season_summary, aes(x = season, y = total_orders_with_discount , fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_orders_with_discount), vjust = 2,color = "black", ) +  # Hiển thị số liệu trên đầu thanh
  theme_minimal() +
  labs(
    title = "Số đơn hàng áp dụng mã khuyến mãi theo từng mùa",
    x = " ",
    y = ""
  ) +
  scale_fill_brewer(palette = "Set3")


# 6.Thống kê suy diễn.

# ---------------- Krusal-wallis Normal Test -----------------
get_order_price <- merged_data$order_price
# Kiểm tra dữ liệu xem có phải phân phối chuẩn không : Dùng shapiro
shapiro_order_price <- shapiro.test(get_order_price)
# Tạo danh sách các mùa
seasons <- c("spring", "summer", "fall", "winter")
# Tạo một danh sách trống để lưu kết quả
order_price_by_season <- list()
# Vòng lặp để trích xuất dữ liệu
for (season in seasons) {
  # Lọc dữ liệu theo mùa
  season_data <- subset(merged_data, season == season)
  # Lấy cột order_price và lưu vào danh sách
  order_price_by_season[[season]] <- season_data$order_price
}
shapiro_summary <- data.frame(
  Season = names(order_price_by_season), # Tên các mùa
  P_value = sapply(order_price_by_season, function(order_price) {
    shapiro.test(order_price)$p.value # Lấy p-value
  })
)

# Thêm cột để diễn giải kết quả
shapiro_summary$Normal_Distribution <- ifelse(shapiro_summary$P_value > 0.05, 
                                              "True", 
                                              "False")

# Xem bảng kết quả từng với bảng (gt)
  
shapiro_summary %>%
gt() %>%
  tab_header(title=md("#### Kết quả phân tích phân phối chuẩn từng mùa"))
## Kết quả: Không phải là phân phối chuẩn.
kruskal_test <- kruskal.test(order_total ~ season, data = merged_data)
# In kết quả kiểm định Kruskal-Wallis
print(kruskal_test)
### ----------------------------------------------------------------


# Phương pháp 2

# Tách 2 cột order_total và season ra data_frame mới
total_season <- merged_data[, c("season", "order_total")] 
# Lấy 200 mẫu (100 spring, 100 summer):
sampled_Wilcoxon_rank_sum <- merged_data %>%
  filter(season %in% c("spring", "summer")) %>%  # lọc ra 2 nhóm spring và summer trong cột season
  arrange(factor(season, levels = c("spring", "summer"))) %>%   # Sắp xếp dữ liệu spring trước và summer sau
  group_by(season) %>%  # nhóm ở cột season
  slice_sample(n = 100) %>% # lấy 100 mẫu bất kỳ
  ungroup() # hủy nhóm để tránh gây lỗi

Wilcoxon_rank_sum_result <- wilcox.test(order_total ~ season, data = sampled_Wilcoxon_rank_sum) # Thực hiện kiểm định Wilcoxon_rank_sum
print(Wilcoxon_rank_sum_result)  # In kết quả kiểm định Wilcoxon_rank_sum

# Phương pháp 3: Wilcoxon Signed rank
total_price <-  merged_data[, c("order_price",  "order_total")] # Tạo một dataframe mới cho riêng order_price và order_total
Wilcoxon_signed_rank_sample <- total_price %>%
  slice_sample(n = 200) # Lấy 200 mẫu bất kì từ bộ dữ liệu
Wilcoxon_signed_rank_result <- wilcox.test(Wilcoxon_signed_rank_sample$order_price, Wilcoxon_signed_rank_sample$order_total, paired = TRUE)  # Kiểm định Wilcoxon_signed_rank
print(Wilcoxon_signed_rank_result) # In kết quả 

# 7: Thống kê và mở rộng
# ---------------- Dunn Test -----------------
dunn_result <- dunn.test(merged_data$order_price, merged_data$season, method = "bonferroni", list = TRUE)

# Tạo bảng kết quả
table <- cbind.data.frame(
  Comparison = dunn_result$comparisons,  # Các cặp so sánh (cụ thể mùa)
  Z_value = dunn_result$Z,               # Giá trị Z
  P_adjusted = dunn_result$P.adjusted    # P-value đã điều chỉnh
)

# Sắp xếp bảng theo p-value đã điều chỉnh
table <- table[order(table$P_adjusted), ]

# Tạo bảng với gt và thêm tiêu đề
table %>%
  gt() %>%
  tab_header(
    title = md("#### Kết quả phân tích pos-hoc (Dunn's Test)"),
    subtitle = "So sánh sự khác nhau giữa các nhóm mùa bằng Bonerroni"
  )

difference <- Wilcoxon_signed_rank_sample$order_price - Wilcoxon_signed_rank_sample$order_total # Tính phép toán trừ giữa order_price và order_total và lưu kết quả vào một đối tượng
summary(difference) # Gọi hàm summary ra các giá trị quan trắc

# --------------------------------------------
