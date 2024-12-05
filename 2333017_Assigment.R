# 3.Tiền xử lý dữ liệu
# 3.1 Ghép dữ liệu
# Load thư viện sử dụng và nối hai file CSV lại
required_packages <- c("this.path", "dplyr", "ggplot2", "lubridate", "geosphere", "readr", "corrplot", "faraway", "car", "ggthemes","gt","nortest","knitr","FSA","ggcorrplot","dunn.test")
for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
# Load đường dẫn hiện tại của thư mục data chứa các file CSV mẫu
setwd(this.path::here())
dirty_data <- read_csv("data/dirty_data.csv")
missing_data <- read_csv("data/missing_data.csv")
# Chuyển định dạng tháng ngày năm cột date
dirty_data$date <- parse_date_time(dirty_data$date, orders = c("mdy", "ymd", "dmy"))
missing_data$date <- parse_date_time(missing_data$date, orders = c("mdy", "ymd", "dmy"))
merged_data <- rbind(dirty_data, missing_data)
# ------------------------------------------------
# 3.2. Hiện thị giá trị bị khuyết sau khi ghép dữ liệu
na_cout<- colSums(is.na(merged_data ))
print(na_cout)

# ------------------------------------------------
# 3.3.1 Xử lý cột order_price và order_total
merged_data <- merged_data %>%
  mutate(
    order_total = ifelse(is.na(order_total), order_price * (100 - coupon_discount) / 100 + delivery_charges, order_total),
    order_price = ifelse(is.na(order_price), (order_total - delivery_charges) * 100 / (100 - coupon_discount), order_price)
)
# ------------------------------------------------
# 3.3.2 Cột season ban đầu
season_unique_before<-unique(merged_data$season)
print(season_unique_before)
# ------------------------------------------------
# 3.3.2 Làm sạch cột season
merged_data$season <- tolower(merged_data$season) # Đổi tất cả giá trị mùa dạng viết thường
month_value <- month(merged_data$date) # lấy tháng trong cột $date
merged_data <- merged_data %>%
  # Toán tử %>% : Truyền kết quả của phép toán hoặc hàm vào hàm tiếp theo .
  # mutate: Tạo ra cột mới hoặc thay đổi giá trị các cột trong data frame
  mutate(season = case_when(
    !is.na(season) ~ season,
    month_value %in% c(12, 1, 2) ~ "winter",
    month_value %in% c(3, 4, 5) ~ "spring",
    month_value %in% c(6, 7, 8) ~ "summer",
    TRUE ~ "autumn"
  ))
# ------------------------------------------------
# 3.3.2 Kiểm tra giá trị cột season sau khi xử lý
season_unique_after<-unique(merged_data$season)
print(season_unique_after)

# ------------------------------------------------
# 3.3.3 Xử lý cột is_happy_customer
median_happy_customer <- round(median(merged_data$is_happy_customer, na.rm = TRUE), digits = 0)
merged_data$is_happy_customer[is.na(merged_data$is_happy_customer)] <- median_happy_customer

# -----------------------------------------------
# 3.2.4 Sau khi làm sạch
na_cout<- colSums(is.na(merged_data ))
print(na_cout)
