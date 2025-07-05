# Nalini Mahastuti
# 5003221157
# Calendar Variation Model with Stochastic Trend (ARIMA) and 
# Deterministic Seasonality (Dummy Variables)

# libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(tidyr)

# load dan praproses data
bulan_indo <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni",
                "Juli", "Agustus", "September", "Oktober", "November", "Desember")

data <- read_excel("data1.xlsx") %>%
  mutate(
    Bulan_num = match(Bulan, bulan_indo),
    Date = as.Date(paste(Tahun, Bulan_num, "01", sep = "-")),
    Total_Penumpang = as.numeric(`Total Penumpang`)
  ) %>%
  filter(!is.na(Date), !is.na(Total_Penumpang)) %>%
  arrange(Date)

# dummy idul fitri
dummy_lebaran <- data.frame(
  Tahun = 2006:2025,
  Bulan_IdulFitri = c(10,10,10,9,9,8,8,8,7,7,7,6,6,6,5,5,5,4,4,3)
) %>%
  mutate(Date_Lebaran = as.Date(paste(Tahun, Bulan_IdulFitri, "01", sep = "-")))

data <- data %>%
  left_join(dummy_lebaran, by = "Tahun") %>%
  mutate(
    month_diff_fitr = interval(Date_Lebaran, Date) %/% months(1),
    IdulFitri       = ifelse(month_diff_fitr == 0, 1, 0),
    Pre1_IdulFitri  = ifelse(month_diff_fitr == -1, 1, 0),
    Post1_IdulFitri = ifelse(month_diff_fitr == 1, 1, 0)
  )

# dummy idul adha
dummy_adha <- data.frame(
  Tahun = 2006:2025,
  Bulan_IdulAdha = c(12,12,12,11,11,11,10,10,10,10,8,8,8,7,7,7,6,6,6,6)
) %>%
  mutate(Date_Adha = as.Date(paste(Tahun, Bulan_IdulAdha, "01", sep = "-")))

data <- data %>%
  left_join(dummy_adha, by = "Tahun") %>%
  mutate(
    month_diff_adha = interval(Date_Adha, Date) %/% months(1),
    IdulAdha        = ifelse(month_diff_adha == 0, 1, 0),
    Pre1_IdulAdha   = ifelse(month_diff_adha == -1, 1, 0),
    Post1_IdulAdha  = ifelse(month_diff_adha == 1, 1, 0)
  )

# dummy lain
data <- data %>%
  mutate(
    NatalTahunBaru  = ifelse(month(Date) %in% c(12, 1), 1, 0),
    LiburSekolah    = ifelse(month(Date) %in% c(6, 7), 1, 0),
    Covid           = ifelse(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-12-01"), 1, 0)
  )

# split data
n <- nrow(data)
train_size <- floor(0.9 * n)

train_data <- data[1:train_size, ]
test_data  <- data[(train_size + 1):n, ]

ts_train <- ts(train_data$Total_Penumpang,
               start = c(year(train_data$Date[1]), month(train_data$Date[1])),
               frequency = 12)

# sarima
auto_model <- auto.arima(ts_train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, trace = TRUE)
summary(auto_model)

# dummy
dummy_vars_all <- c("Pre1_IdulFitri", "IdulFitri", "Post1_IdulFitri",
                    "Pre1_IdulAdha",  "IdulAdha",  "Post1_IdulAdha",
                    "NatalTahunBaru", "LiburSekolah", "Covid")
dummy_vars_used <- dummy_vars_all[sapply(dummy_vars_all, function(v) {
  length(unique(train_data[[v]])) > 1
})]
cor_matrix <- cor(train_data[, dummy_vars_used])
print(round(cor_matrix, 2))

# Hitung p-value awal semua dummy
xreg_init <- as.matrix(train_data[, dummy_vars_used])
fit_init <- Arima(ts_train, order = c(1,1,1),
                  seasonal = list(order = c(0,0,2), period = 12),
                  xreg = xreg_init, include.drift = TRUE)

coefs_init <- coef(fit_init)
se_init <- sqrt(diag(fit_init$var.coef))
z_init <- coefs_init[dummy_vars_used] / se_init[dummy_vars_used]
p_init <- 2 * (1 - pnorm(abs(z_init)))

# Tampilkan p-value awal
cat("📊 P-value awal dummy variables:\n")
pval_table <- data.frame(
  Variable = names(p_init),
  P_value = round(p_init, 4)
)
print(pval_table[order(pval_table$P_value, decreasing = TRUE), ])

# backward elim
current_vars <- dummy_vars_used
threshold <- 0.005
done <- FALSE

while (!done && length(current_vars) > 0) {
  xreg_sub <- as.matrix(train_data[, current_vars])
  
  fit <- tryCatch({
    Arima(ts_train, order = c(1,1,1),
          seasonal = list(order = c(0,0,2), period = 12),
          xreg = xreg_sub, include.drift = TRUE)
  }, error = function(e) {
    cat("❌ Error fitting model:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(fit)) break
  
  coefs <- coef(fit)
  se <- sqrt(diag(fit$var.coef))
  z <- coefs[current_vars] / se[current_vars]
  p <- 2 * (1 - pnorm(abs(z)))
  
  max_p <- max(p, na.rm = TRUE)
  if (max_p > threshold) {
    drop_var <- names(p)[which.max(p)]
    cat("🔻 Drop:", drop_var, "(p =", round(max_p, 4), ")\n")
    current_vars <- setdiff(current_vars, drop_var)
  } else {
    cat("✅ Semua variabel signifikan!\n")
    done <- TRUE
  }
}

cat("\n📌 Variabel dummy final:\n")
print(current_vars)

# final model
xreg_train <- as.matrix(train_data[, current_vars])
xreg_test  <- as.matrix(test_data[, current_vars])
colnames(xreg_test) <- NULL

fit_final <- Arima(ts_train, order = c(1,1,1),
                   seasonal = list(order = c(0,0,2), period = 12),
                   xreg = xreg_train, include.drift = TRUE)

summary(fit_final)

# predict & eval
forecast_test <- forecast(fit_final, xreg = xreg_test, h = nrow(test_data))
actual_test <- test_data$Total_Penumpang

plot_test_df <- data.frame(
  Date = test_data$Date,
  Aktual = actual_test,
  Prediksi = as.numeric(forecast_test$mean)
)
plot_test_long <- pivot_longer(plot_test_df, cols = c("Aktual", "Prediksi"), names_to = "Tipe", values_to = "Jumlah")
ggplot(plot_test_long, aes(x = Date, y = Jumlah, color = Tipe)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Aktual" = "orange", "Prediksi" = "darkgreen")) +
  labs(
    title = "Perbandingan Prediksi vs Aktual (Data Uji)",
    x = "Tanggal", y = "Jumlah Penumpang", color = "Keterangan"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# error
errors <- actual_test - forecast_test$mean
cat("MAE  =", round(mean(abs(errors)), 2), "\n")
cat("MSE  =", round(mean(errors^2), 2), "\n")
cat("RMSE =", round(sqrt(mean(errors^2)), 2), "\n")
cat("MAPE =", round(mean(abs(errors / actual_test)) * 100, 2), "%\n")

# prediksi 2025 mei–desember)
dummy_2025 <- data.frame(
  Date = seq(as.Date("2025-05-01"), by = "month", length.out = 8)
) %>%
  mutate(
    Bulan = month(Date),
    Pre1_IdulFitri  = ifelse(Bulan == 2, 1, 0),
    IdulFitri       = ifelse(Bulan == 3, 1, 0),
    Post1_IdulFitri = ifelse(Bulan == 4, 1, 0),
    Pre1_IdulAdha   = ifelse(Bulan == 5, 1, 0),
    IdulAdha        = ifelse(Bulan == 6, 1, 0),
    Post1_IdulAdha  = ifelse(Bulan == 7, 1, 0),
    NatalTahunBaru  = ifelse(Bulan %in% c(12, 1), 1, 0),
    LiburSekolah    = ifelse(Bulan %in% c(6, 7), 1, 0),
    Covid           = 0
  )

xreg_2025 <- as.matrix(dummy_2025[, current_vars])
colnames(xreg_2025) <- NULL

# model full + prediksi 2025
ts_full <- ts(data$Total_Penumpang,
              start = c(year(data$Date[1]), month(data$Date[1])),
              frequency = 12)
xreg_full <- as.matrix(data[, current_vars])
colnames(xreg_full) <- NULL

fit_full <- Arima(ts_full, order = c(1,1,1),
                  seasonal = list(order = c(0,0,2), period = 12),
                  xreg = xreg_full)

forecast_2025 <- forecast(fit_full, xreg = xreg_2025, h = 8)

# visualisasi gabungan
train_df <- data.frame(Date = train_data$Date, value = ts_train, type = "Latih")
test_df  <- data.frame(Date = test_data$Date, value = actual_test, type = "Aktual (Uji)")
forecast_test_df <- data.frame(Date = test_data$Date, value = as.numeric(forecast_test$mean), type = "Prediksi (Uji)")
forecast_2025_df <- data.frame(Date = dummy_2025$Date, value = as.numeric(forecast_2025$mean), type = "Prediksi 2025")

plot_data <- bind_rows(train_df, test_df, forecast_test_df, forecast_2025_df)

ggplot(plot_data, aes(x = Date, y = value, color = type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  scale_color_manual(values = c(
    "Latih" = "steelblue", 
    "Aktual (Uji)" = "orange", 
    "Prediksi (Uji)" = "darkgreen",
    "Prediksi 2025" = "purple"
  )) +
  labs(title = "Prediksi vs Aktual (Model ARIMAX)",
       x = NULL, y = "Jumlah Penumpang", color = "Keterangan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

forecast_2025_df
