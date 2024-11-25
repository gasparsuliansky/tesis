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


# Mean Reversion

# Set window size for the rolling mean and standard deviation
window <- 50

# Calculate rolling mean and standard deviation
merval <- merval %>%
  mutate(rolling_mean = rollmean(adjusted, window, fill = NA, align = "right"),
         rolling_sd = rollapply(adjusted, width = window, FUN = sd, fill = NA, align = "right"))

# Calculate Z-score
merval <- merval %>%
  mutate(z_score = (adjusted - rolling_mean) / rolling_sd)

# Generate buy (Z-score < -1.5) and sell (Z-score > 1.5) signals
merval <- merval %>%
  mutate(signal = case_when(
    z_score < -1.5 ~ 1,  # Buy when Z-score < -1.5 (oversold)
    z_score > 1.5  ~ -1, # Sell when Z-score > 1.5 (overbought)
    TRUE ~ 0       # Hold otherwise
  ))

# Shift signal by one day to represent actual trading action
merval <- merval %>%
  mutate(trade_signal = lag(signal)) %>%
  na.omit()

# Calculate strategy returns based on signals
merval <- merval %>%
  mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                  ifelse(trade_signal == -1, -return, 0)))

# Compute cumulative returns for both buy-and-hold and mean-reversion strategy
merval <- merval %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1,
         cumulative_strategy_return = cumprod(1 + strategy_return) -1)

# Final return calculations
final_market_return <- last(merval$cumulative_market_return)
final_strategy_return <- last(merval$cumulative_strategy_return)

# Print the final returns
cat("Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
cat("Final Mean-Reversion Strategy Return: ", round(final_strategy_return * 100, 2), "%\n")

# Create a ggplot of cumulative returns
ggplot(merval, aes(x = date)) +
  geom_line(aes(y = cumulative_market_return, color = "Buy-and-Hold"), size = 0.4) +
  geom_line(aes(y = cumulative_strategy_return, color = "MR"), size = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black", size = 0.1) +
  scale_color_manual(values = c("Buy-and-Hold" = "darkblue", "MR" = "forestgreen")) +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.margin = margin(t = -15))
ggsave("merval mr.pdf", dpi = 320, width = 4, height = 3)
dev.off()

# Plot the cumulative returns using base R
plot(merval$date, merval$cumulative_market_return, type = "l", col = "steelblue3", lwd = 2,
     xlab = "Date", ylab = "Cumulative Return", main = "Cumulative Returns: Mean-Reversion Strategy vs Buy-and-Hold")
lines(merval$date, merval$cumulative_strategy_return, col = "darkorange", lwd = 2)
legend("topleft", legend = c("Buy-and-Hold", "Mean-Reversion Strategy"),
       col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)

dev.off()


# Loop through each period dataset in merval_list and process them
for (i in 1:length(merval_list)) {
  
  # Extract current period dataset
  merval_period <- merval_list[[i]]
  
  # Calculate rolling mean and standard deviation
  merval_period <- merval_period %>%
    mutate(rolling_mean = rollmean(adjusted, window, fill = NA, align = "right"),
           rolling_sd = rollapply(adjusted, width = window, FUN = sd, fill = NA, align = "right")) %>%
    na.omit()  # Ensure no missing values
  
  # Calculate Z-score
  merval_period <- merval_period %>%
    mutate(z_score = (adjusted - rolling_mean) / rolling_sd) %>%
    na.omit()  # Ensure no missing values
  
  # Generate buy (Z-score < -1.5) and sell (Z-score > 1.5) signals
  merval_period <- merval_period %>%
    mutate(signal = case_when(
      z_score < -1.5 ~ 1,  # Buy when Z-score < -1.5 (oversold)
      z_score > 1.5  ~ -1, # Sell when Z-score > 1.5 (overbought)
      TRUE ~ 0       # Hold otherwise
    )) %>%
    na.omit()  # Ensure no missing values
  
  # Shift signal by one day to represent actual trading action
  merval_period <- merval_period %>%
    mutate(trade_signal = lag(signal)) %>%
    na.omit()
  
  # Calculate strategy returns based on signals
  merval_period <- merval_period %>%
    mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                    ifelse(trade_signal == -1, -return, 0)))
  
  # Compute cumulative returns for both buy-and-hold and mean-reversion strategy
  merval_period <- merval_period %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1,
           cumulative_strategy_return = cumprod(1 + strategy_return) - 1)
  
  # Final return calculations
  final_market_return <- last(merval_period$cumulative_market_return)
  final_strategy_return <- last(merval_period$cumulative_strategy_return)
  
  # Print the final returns for the current period
  cat("Period:", periods[i], "\n")
  cat("Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
  cat("Final Mean-Reversion Strategy Return: ", round(final_strategy_return * 100, 2), "%\n\n")
  
  # Plot the cumulative returns for the current period
  plot(merval_period$date, merval_period$cumulative_market_return, type = "l", col = "steelblue3", lwd = 2, ylim = c(-1, 2),
       xlab = "Date", ylab = "Cumulative Return", main = paste("Cumulative Returns: Mean-Reversion Strategy vs Buy-and-Hold (", periods[i], ")", sep=""))
  lines(merval_period$date, merval_period$cumulative_strategy_return, col = "darkorange", lwd = 2)
  legend("topleft", legend = c("Buy-and-Hold", "Mean-Reversion Strategy"),
         col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)
}

dev.off()

rm(list = ls())
