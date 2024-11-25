# Load necessary libraries
library(dplyr)
library(tidyquant)
library(tidyverse)

# Data

# Get MERVAL data from 1980-01-01 to 2020-01-01
merval <- tq_get("^MERV", from = "1980-01-01", to = "2020-01-01")

# Compute daily log returns
merval <- merval %>%
  mutate(return = adjusted / lag(adjusted) - 1,
         log_return = c(NA, diff(log(adjusted)))) %>%
  na.omit()

# Filter the data into specific periods
merval_1 <- filter(merval, date < as.Date("2000-01-01"))
merval_2 <- filter(merval, date >= as.Date("2000-01-01") & date < as.Date("2005-01-01"))
merval_3 <- filter(merval, date >= as.Date("2005-01-01") & date < as.Date("2010-01-01"))
merval_4 <- filter(merval, date >= as.Date("2010-01-01") & date < as.Date("2015-01-01"))
merval_5 <- filter(merval, date >= as.Date("2015-01-01") & date < as.Date("2020-01-01"))

# Combine the data into a list and define the periods
merval_list <- list(merval_1, merval_2, merval_3, merval_4, merval_5)
periods <- c("1996-2000", "2000-2005", "2005-2010", "2010-2015", "2015-2020")


# Plots

par(mfrow = c(1, 3))

plot(merval$date, merval$adjusted, type = "l", col = "darkblue", 
     main = "Merval Adjusted Prices (1980-2020)", 
     ylab = "Price", xlab = "Date")

plot(merval$date, merval$return, type = "l", col = "darkblue", 
     main = "Merval Returns (1980-2020)", 
     ylab = "Returns", xlab = "Date")
abline(h = 0, col = "black", lty = 2, lwd = 1)

plot(merval$date, merval$log_return, type = "l", col = "darkblue", 
     main = "Merval Log Returns (1980-2020)", 
     ylab = "Log Returns", xlab = "Date")
abline(h = 0, col = "black", lty = 2, lwd = 1)

dev.off()


# Loop over each dataset and create the plots
for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the current dataset
  period <- periods[i]  # Get the current period
  
  par(mfrow = c(1, 3))  # Arrange 3 plots in one row
  
  # Plot 1: Adjusted Prices
  plot(merval_period$date, merval_period$adjusted, type = "l", col = "darkblue", 
       main = paste("Merval Adjusted Prices", period), 
       ylab = "Price", xlab = "Date")
  
  # Plot 2: Returns
  plot(merval_period$date, merval_period$return, type = "l", col = "darkblue", 
       main = paste("Merval Returns", period), 
       ylab = "Returns", xlab = "Date")
  abline(h = 0, col = "black", lty = 2, lwd = 1)
  
  # Plot 3: Log Returns
  plot(merval_period$date, merval_period$log_return, type = "l", col = "darkblue", 
       main = paste("Merval Log Returns", period), 
       ylab = "Log Returns", xlab = "Date")
  abline(h = 0, col = "black", lty = 2, lwd = 1)
}

dev.off()


# Plot the data
ggplot(merval, aes(date, adjusted)) +
  geom_vline(xintercept = as.Date(paste0(seq(1980, 2020, by = 5), "-01-01")), 
             linetype = "dashed", color = "gray") +
  geom_line(col = "darkblue") +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("merval adjusted.pdf", dpi = 320, width = 4, height = 3)
dev.off()

ggplot(merval, aes(date, log_return)) +
  geom_vline(xintercept = as.Date(paste0(seq(1980, 2020, by = 5), "-01-01")), 
             linetype = "dashed", color = "gray") +
  geom_line(col = "darkblue") +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("merval log returns.pdf", dpi = 320, width = 4, height = 3)
dev.off()

ggplot(merval, aes(x = log_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 300, fill = "darkblue") +
  ggthemes::theme_base() +
  labs(x = "", y = "Densidad") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","), limits = c(-0.15, 0.15)) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("merval log returns distribution.pdf", dpi = 320, width = 4, height = 3)
dev.off()


# Exploratory Data Analysis

observations <- length(merval$adjusted)
mean_return <- mean(merval$log_return, na.rm = TRUE)
median_return <- median(merval$log_return, na.rm = TRUE)
var_return <- var(merval$log_return, na.rm = TRUE)
sd_return <- sd(merval$log_return, na.rm = TRUE)
cv_return <- sd_return/mean_return
min_return <- min(merval$log_return, na.rm = TRUE)
max_return <- max(merval$log_return, na.rm = TRUE)
count_return <- length(merval$log_return)
kurtosis_return <- moments::kurtosis(merval$log_return, na.rm = TRUE)
skewness_return <- moments::skewness(merval$log_return, na.rm = TRUE)

# Display the results
cat("Observations:", observations, "\n")
cat("Mean log return:", mean_return, "\n")
cat("Median log return:", median_return, "\n")
cat("Variance of log return:", var_return, "\n")
cat("Standard deviation of log return:", sd_return, "\n")
cat("Coefficient of variation:", cv_return, "\n")
cat("Min log return:", min_return, "\n")
cat("Max log return:", max_return, "\n")
cat("Number of outliers:", outlier_count, "\n")
cat("Kurtosis of log return:", kurtosis_return, "\n")
cat("Skewness of log return:", skewness_return, "\n")

for (i in 1:length(merval_list)) {
  
  cat("\n----- EDA for period:", periods[i], "-----\n")
  
  # Get the current period data
  merval_data <- merval_list[[i]]
  
  # Compute detailed summary statistics
  mean_return <- mean(merval_data$log_return, na.rm = TRUE)
  median_return <- median(merval_data$log_return, na.rm = TRUE)
  sd_return <- sd(merval_data$log_return, na.rm = TRUE)
  var_return <- var(merval_data$log_return, na.rm = TRUE)
  min_return <- min(merval_data$log_return, na.rm = TRUE)
  max_return <- max(merval_data$log_return, na.rm = TRUE)
  count_return <- length(merval_data$log_return)
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
  hist(merval_data$log_return, breaks = 50, probability = TRUE, 
       main = paste("Histogram of Log Returns for", periods[i]),
       xlab = "Log Returns", col = "darkblue", border = "black")
  lines(density(merval_data$log_return, na.rm = TRUE), col = "black", lwd = 2)
  
}

dev.off()

rm(list = ls())
