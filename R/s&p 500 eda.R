# Load necessary libraries
library(dplyr)
library(tidyquant)
library(tidyverse)

# Data

# Get S&P 500 data from 1980-01-01 to 2020-01-01
sp <- tq_get("^GSPC", from = "1980-01-01", to = "2020-01-01")

# Compute daily log returns
sp <- sp %>%
  mutate(return = adjusted / lag(adjusted) - 1,
         log_return = log(adjusted / lag(adjusted)))%>%
  na.omit()

# Filter the data into specific periods
sp_1 <- filter(sp, date >= as.Date("1980-01-01") & date < as.Date("1985-01-01"))
sp_2 <- filter(sp, date >= as.Date("1985-01-01") & date < as.Date("1990-01-01"))
sp_3 <- filter(sp, date >= as.Date("1990-01-01") & date < as.Date("1995-01-01"))
sp_4 <- filter(sp, date >= as.Date("1995-01-01") & date < as.Date("2000-01-01"))
sp_5 <- filter(sp, date >= as.Date("2000-01-01") & date < as.Date("2005-01-01"))
sp_6 <- filter(sp, date >= as.Date("2005-01-01") & date < as.Date("2010-01-01"))
sp_7 <- filter(sp, date >= as.Date("2010-01-01") & date < as.Date("2015-01-01"))
sp_8 <- filter(sp, date >= as.Date("2015-01-01") & date < as.Date("2020-01-01"))

# Combine the data into a list and define the periods
sp_list <- list(sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7, sp_8)
periods <- c("1980-1985", "1985-1990", "1990-1995", "1995-2000", "2000-2005", "2005-2010", "2010-2015", "2015-2020")


# Plots

par(mfrow = c(1, 3))

plot(sp$date, sp$adjusted, type = "l", col = "darkred", 
     main = "S&P 500 Adjusted Prices (1980-2020)", 
     ylab = "Price", xlab = "Date")

plot(sp$date, sp$return, type = "l", col = "darkred", 
     main = "S&P 500 Returns (1980-2020)", 
     ylab = "Returns", xlab = "Date")
abline(h = 0, col = "black", lty = 2, lwd = 1)

plot(sp$date, sp$log_return, type = "l", col = "darkred", 
     main = "S&P 500 Log Returns (1980-2020)", 
     ylab = "Log Returns", xlab = "Date")
abline(h = 0, col = "black", lty = 2, lwd = 1)

dev.off()


# Loop over each dataset and create the plots
for (i in 1:length(sp_list)) {
  sp_period <- sp_list[[i]]  # Get the current dataset
  period <- periods[i]  # Get the current period
  
  par(mfrow = c(1, 3))  # Arrange 3 plots in one row
  
  # Plot 1: Adjusted Prices
  plot(sp_period$date, sp_period$adjusted, type = "l", col = "darkred", 
       main = paste("S&P 500 Adjusted Prices", period), 
       ylab = "Price", xlab = "Date")
  
  # Plot 2: Returns
  plot(sp_period$date, sp_period$return, type = "l", col = "darkred", 
       main = paste("S&P 500 Returns", period), 
       ylab = "Returns", xlab = "Date")
  abline(h = 0, col = "black", lty = 2, lwd = 1)
  
  # Plot 3: Log Returns
  plot(sp_period$date, sp_period$log_return, type = "l", col = "darkred", 
       main = paste("S&P 500 Log Returns", period), 
       ylab = "Log Returns", xlab = "Date")
  abline(h = 0, col = "black", lty = 2, lwd = 1)
}

dev.off()


# Plot the data
ggplot(sp, aes(date, adjusted)) +
  geom_vline(xintercept = as.Date(paste0(seq(1980, 2020, by = 5), "-01-01")), 
             linetype = "dashed", color = "gray") +
  geom_line(col = "darkred") +
  ggthemes::theme_base() +
  labs(x = element_blank(), y = element_blank()) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("s&p adjusted.pdf", dpi = 320, width = 4, height = 3)
dev.off()

ggplot(sp, aes(date, log_return)) +
  geom_vline(xintercept = as.Date(paste0(seq(1980, 2020, by = 5), "-01-01")), 
             linetype = "dashed", color = "gray") +
  geom_line(col = "darkred") +
  ggthemes::theme_base() +
  labs(x = element_blank(), y = element_blank()) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("s&p log returns.pdf", dpi = 320, width = 4, height = 3)
dev.off()

ggplot(sp, aes(x = log_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 300, fill = "darkred") +
  ggthemes::theme_base() +
  labs(x = "", y = "Densidad") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","), limits = c(-0.15, 0.15)) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("s&p log returns distribution.pdf", dpi = 320, width = 4, height = 3)
dev.off()


# Exploratory Data Analysis

observations <- length(sp$adjusted)
mean_return <- mean(sp$log_return, na.rm = TRUE)
median_return <- median(sp$log_return, na.rm = TRUE)
sd_return <- sd(sp$log_return, na.rm = TRUE)
var_return <- var(sp$log_return, na.rm = TRUE)
cv_return <- sd_return/mean_return
min_return <- min(sp$log_return, na.rm = TRUE)
max_return <- max(sp$log_return, na.rm = TRUE)
count_return <- length(sp$log_return)
kurtosis_return <- moments::kurtosis(sp$log_return, na.rm = TRUE)
skewness_return <- moments::skewness(sp$log_return, na.rm = TRUE)

# Display the results
cat("Observations:", observations, "\n")
cat("Mean log return:", mean_return, "\n")
cat("Median log return:", median_return, "\n")
cat("Variance of log return:", var_return, "\n")
cat("Standard deviation of log return:", sd_return, "\n")
cat("Coefficient of variation:", cv_return, "\n")
cat("Min log return:", min_return, "\n")
cat("Max log return:", max_return, "\n")
cat("Kurtosis of log return:", kurtosis_return, "\n")
cat("Skewness of log return:", skewness_return, "\n")

for (i in 1:length(sp_list)) {
  
  cat("\n----- EDA for period:", periods[i], "-----\n")
  
  # Get the current period data
  sp_data <- sp_list[[i]]
  
  # Compute detailed summary statistics
  mean_return <- mean(sp_data$log_return, na.rm = TRUE)
  median_return <- median(sp_data$log_return, na.rm = TRUE)
  sd_return <- sd(sp_data$log_return, na.rm = TRUE)
  var_return <- var(sp_data$log_return, na.rm = TRUE)
  min_return <- min(sp_data$log_return, na.rm = TRUE)
  max_return <- max(sp_data$log_return, na.rm = TRUE)
  count_return <- length(sp_data$log_return)
  kurtosis_return <- moments::kurtosis(sp_data$log_return, na.rm = TRUE)
  skewness_return <- moments::skewness(sp_data$log_return, na.rm = TRUE)
  
  # Display the results
  cat("Mean log return:", mean_return, "\n")
  cat("Median log return:", median_return, "\n")
  cat("Standard deviation of log return:", sd_return, "\n")
  cat("Variance of log return:", var_return, "\n")
  cat("Min log return:", min_return, "\n")
  cat("Max log return:", max_return, "\n")
  cat("Kurtosis of log return:", kurtosis_return, "\n")
  cat("Skewness of log return:", skewness_return, "\n")
  
  # Histogram with density
  hist(sp_data$log_return, breaks = 50, probability = TRUE, 
       main = paste("Histogram of Log Returns for", periods[i]),
       xlab = "Log Returns", col = "darkred", border = "black")
  lines(density(sp_data$log_return, na.rm = TRUE), col = "black", lwd = 2)
  
}

dev.off()

rm(list = ls())
