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
         log_return = log(adjusted / lag(adjusted))) %>%
  na.omit()

# Mean Reversion

# Set window size for the rolling mean and standard deviation
window <- 50

# Calculate rolling mean and standard deviation
sp <- sp %>%
  mutate(rolling_mean = rollmean(adjusted, window, fill = NA, align = "right"),
         rolling_sd = rollapply(adjusted, width = window, FUN = sd, fill = NA, align = "right"))

# Calculate Z-score
sp <- sp %>%
  mutate(z_score = (adjusted - rolling_mean) / rolling_sd)

# Define thresholds
thresholds <- c(1, 1.5)
threshold_labels <- c("|1|", "|1,5|")

# Initialize data frame for cumulative returns
strategy_returns <- sp %>% select(date, return) %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1)

# Loop through thresholds
for (i in seq_along(thresholds)) {
  thresh <- thresholds[i]
  label <- threshold_labels[i]
  
  # Generate buy (Z-score < -threshold) and sell (Z-score > threshold) signals
  sp_temp <- sp %>% mutate(signal = case_when(
    z_score < -thresh ~ 1,  # Buy when Z-score < -threshold (oversold)
    z_score > thresh  ~ -1, # Sell when Z-score > threshold (overbought)
    TRUE ~ 0       # Hold otherwise
  )) %>% mutate(trade_signal = lag(signal)) %>%
    na.omit() %>% mutate(strategy_return = ifelse(
      trade_signal == 1, return,
      ifelse(trade_signal == -1, -return, 0)
    )) %>% mutate(
      cumulative_strategy_return = cumprod(1 + strategy_return) - 1,
      cumulative_market_return = cumprod(1 + return) - 1
    )
  
  strategy_returns <- strategy_returns %>% left_join(
    sp_temp %>% select(date, cumulative_strategy_return),
    by = "date"
  ) %>% rename(!!paste0("Strategy_", label) := cumulative_strategy_return)
  
  final_market_return <- last(sp_temp$cumulative_market_return)
  final_strategy_return <- last(sp_temp$cumulative_strategy_return)
  
  cat("Final B&H Return: ", round(final_market_return * 100, 2), "%\n")
  cat(sprintf("Final Mean-Reversion Strategy (%s) Return: ", label), round(final_strategy_return * 100, 2), "%\n\n")
}

# Prepare data for plotting
plot_data <- strategy_returns %>% select(date, cumulative_market_return, starts_with("Strategy_")) %>%
  pivot_longer(cols = -date, names_to = "Strategy", values_to = "Cumulative_Return")

# Clean up Strategy names
plot_data$Strategy <- gsub("Strategy_", "", plot_data$Strategy)
plot_data$Strategy <- ifelse(plot_data$Strategy == "cumulative_market_return", "B&H", plot_data$Strategy)

# Create a ggplot of cumulative returns
ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Strategy, linetype = Strategy)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c("B&H" = "darkred", "|1|" = "darkgreen", "|1,5|" = "darkgreen")) +
  scale_linetype_manual(values = c("B&H" = "solid", "|1|" = "dashed", "|1,5|" = "dotted")) +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank(),
        legend.title = element_blank(), legend.position = "bottom", legend.margin = margin(t = -15))
ggsave("s&p mr F.pdf", dpi = 320, width = 4, height = 3)
dev.off()


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

# Loop through each sub-period
for (j in seq_along(sp_list)) {
  sp <- sp_list[[j]]
  period <- periods[j]
  cat("Period:", period, "\n")
  
  # Calculate rolling mean and standard deviation
  sp <- sp %>%
    mutate(rolling_mean = rollmean(adjusted, window, fill = NA, align = "right"),
           rolling_sd = rollapply(adjusted, width = window, FUN = sd, fill = NA, align = "right"))
  
  # Calculate Z-score
  sp <- sp %>%
    mutate(z_score = (adjusted - rolling_mean) / rolling_sd)
  
  # Initialize data frame for cumulative returns
  strategy_returns <- sp %>% select(date, return) %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1)
  
  # Loop through thresholds
  for (i in seq_along(thresholds)) {
    thresh <- thresholds[i]
    label <- threshold_labels[i]
    
    # Generate buy (Z-score < -threshold) and sell (Z-score > threshold) signals
    sp_temp <- sp %>% mutate(signal = case_when(
      z_score < -thresh ~ 1,  # Buy when Z-score < -threshold (oversold)
      z_score > thresh  ~ -1, # Sell when Z-score > threshold (overbought)
      TRUE ~ 0                # Hold otherwise
    )) %>% mutate(trade_signal = lag(signal)) %>%
      na.omit() %>% mutate(strategy_return = ifelse(
        trade_signal == 1, return,
        ifelse(trade_signal == -1, -return, 0)
      )) %>% mutate(
        cumulative_strategy_return = cumprod(1 + strategy_return) - 1,
        cumulative_market_return = cumprod(1 + return) - 1
      )
    
    final_market_return <- last(sp_temp$cumulative_market_return)
    final_strategy_return <- last(sp_temp$cumulative_strategy_return)
    
    cat("  Threshold:", label, "\n")
    cat("    Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
    cat(sprintf("    Final Mean-Reversion Strategy (%s) Return: ", label), round(final_strategy_return * 100, 2), "%\n\n")
  }
}

dev.off()

rm(list = ls())
