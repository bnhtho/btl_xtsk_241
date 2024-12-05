required_packages <- c("this.path", "dplyr", "ggplot2", "lubridate", "geosphere", "readr", "corrplot", "faraway", "car", "ggthemes","gt","nortest","knitr","FSA","ggcorrplot","dunn.test")
for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
setwd(this.path::here())
dirty_data <- read_csv("data/dirty_data.csv")
missing_data <- read_csv("data/missing_data.csv")
warehouses <- read_csv("data/warehouses.csv")
dirty_data$date <- parse_date_time(dirty_data$date, orders = c("mdy", "ymd", "dmy"))
missing_data$date <- parse_date_time(missing_data$date, orders = c("mdy", "ymd", "dmy"))
merged_data <- rbind(dirty_data, missing_data)
na_cout<- colSums(is.na(merged_data ))
print(na_cout)
merged_data <- merged_data %>%
  mutate(
    order_total = ifelse(is.na(order_total), order_price * (100 - coupon_discount) / 100 + delivery_charges, order_total),
    order_price = ifelse(is.na(order_price), (order_total - delivery_charges) * 100 / (100 - coupon_discount), order_price)
  )
season_unique_before<-unique(merged_data$season)
print(season_unique_before)
merged_data$season <- tolower(merged_data$season)
month_value <- month(merged_data$date)
merged_data <- merged_data %>%
  mutate(season = case_when(
    !is.na(season) ~ season,
    month_value %in% c(12, 1, 2) ~ "winter",
    month_value %in% c(3, 4, 5) ~ "spring",
    month_value %in% c(6, 7, 8) ~ "summer",
    TRUE ~ "autumn"
  ))
season_unique_after<-unique(merged_data$season)
print(season_unique_after)
median_happy_customer <- round(median(merged_data$is_happy_customer, na.rm = TRUE), digits = 0)
merged_data$is_happy_customer[is.na(merged_data$is_happy_customer)] <- median_happy_customer
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Điểm ngoại lai của order_price",
    y = ""
  )
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Điểm ngoại lai của order_total",
    y = ""
  )
quantiles_price <- quantile(merged_data$order_price)
quantiles_total <- quantile(merged_data$order_total)
q1_price <- quantiles_price[2]
q3_price <- quantiles_price[4]
q1_total <- quantiles_total[2]
q3_total <- quantiles_total[4]
IQR_price <- q3_price - q1_price
IQR_total <- q3_total - q1_total
calc_lower <- function(Q1, IQR) { return(Q1 - 1.5 * IQR) }
calc_upper <- function(Q3, IQR) { return(Q3 + 1.5 * IQR) }
lower_price <- calc_lower(q1_price, IQR_price)
upper_price <- calc_upper(q3_price, IQR_price)
lower_total <- calc_lower(q1_total, IQR_total)
upper_total <- calc_upper(q3_total, IQR_total)
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
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Đồ thị thể hiện tứ phân vị của cột Order Price",
  )
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Đồ thị thể hiện tứ phân vị của cột Order Total",
  )
season_summary <- merged_data %>%
  group_by(season) %>%
  summarise(
    total_orders = n(),
    avg_order_total = mean(order_total, na.rm = TRUE),
    total_delivery_charges = sum(delivery_charges, na.rm = TRUE)
  )
print(season_summary)
ggplot(data = season_summary, aes(x = season, y = total_orders, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_orders), vjust = 2,color = "white", ) +
  theme_minimal() +
  labs(
    title = "Tổng số đơn hàng trong từng mùa",
    x = " ",
    y = "Số đơn hàng"
  ) +
  scale_fill_brewer(palette = "Paired")
ggplot(data = season_summary, aes(x = season, y = total_delivery_charges, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_delivery_charges), vjust = 2,color = "white", ) +
  theme_minimal() +
  labs(
    title = "Tổng phí giao hàng từng mùa",
    x = " ",
    y = "Phí giao hàng"
  ) +
  scale_fill_brewer(palette = "Set2")
overview_data <- merged_data[c("order_price", "delivery_charges", "coupon_discount","order_total", "is_expedited_delivery", "is_happy_customer", "shopping_cart")]
numeric_data <- overview_data[sapply(overview_data, is.numeric)]
cor_matrix <- cor(numeric_data)
print(cor_matrix)
corrplot(
  cor_matrix,
  method = "color",
  col= colorRampPalette(c("white","#202020", "#202040"))(10) ,
  type = "full",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "white",
  number.cex = 0.8,
  tl.cex = 0.8,
  diag = TRUE,
  cl.pos = "r"
)
get_order_price <- merged_data$order_price
shapiro_order_price <- shapiro.test(get_order_price)
seasons <- c("spring", "summer", "fall", "winter")
order_price_by_season <- list()
for (season in seasons) {
  season_data <- subset(merged_data, season == season)
  order_price_by_season[[season]] <- season_data$order_price
}
shapiro_summary <- data.frame(
  Season = names(order_price_by_season),
  P_value = sapply(order_price_by_season, function(order_price) {
    shapiro.test(order_price)$p.value
  })
)
shapiro_summary$Normal_Distribution <- ifelse(shapiro_summary$P_value > 0.05, 
                                              "True", 
                                              "False")
shapiro_summary %>%
  gt() %>%
  tab_header(title=md("#### Kết quả phân tích phân phối chuẩn từng mùa"))
kruskal_test <- kruskal.test(order_total ~ season, data = merged_data)
print(kruskal_test)
total_season <- merged_data[, c("season", "order_total")]
sampled_Wilcoxon_rank_sum <- merged_data %>%
  filter(season %in% c("spring", "summer")) %>%
  arrange(factor(season, levels = c("spring", "summer"))) %>%
  group_by(season) %>%
  slice_sample(n = 100) %>%
  ungroup()
Wilcoxon_rank_sum_result <- wilcox.test(order_total ~ season, data = sampled_Wilcoxon_rank_sum)
print(Wilcoxon_rank_sum_result)
total_price <-  merged_data[, c("order_price",  "order_total")]
Wilcoxon_signed_rank_sample <- total_price %>%
  slice_sample(n = 200)
Wilcoxon_signed_rank_result <- wilcox.test(Wilcoxon_signed_rank_sample$order_price, Wilcoxon_signed_rank_sample$order_total, paired = TRUE)
print(Wilcoxon_signed_rank_result)
dunn_result <- dunn.test(merged_data$order_price, merged_data$season, method = "bonferroni", list = TRUE)
table <- cbind.data.frame(
  Comparison = dunn_result$comparisons,
  Z_value = dunn_result$Z,
  P_value = dunn_result$P.adjusted
)
print(table)