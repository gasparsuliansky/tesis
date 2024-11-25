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

# Generate buy (Z-score < -1.5) and sell (Z-score > 1.5) signals
sp <- sp %>%
  mutate(signal = case_when(
    z_score < -1.5 ~ 1,  # Buy when Z-score < -1.5 (oversold)
    z_score > 1.5  ~ -1, # Sell when Z-score > 1.5 (overbought)
    TRUE ~ 0       # Hold otherwise
  ))

# Shift signal by one day to represent actual trading action
sp <- sp %>%
  mutate(trade_signal = lag(signal)) %>%
  na.omit()

# Calculate strategy returns based on signals
sp <- sp %>%
  mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                  ifelse(trade_signal == -1, -return, 0)))

# Compute cumulative returns for both buy-and-hold and mean-reversion strategy
sp <- sp %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1,
         cumulative_strategy_return = cumprod(1 + strategy_return) -1)

# Final return calculations
final_market_return <- last(sp$cumulative_market_return)
final_strategy_return <- last(sp$cumulative_strategy_return)

# Print the final returns
cat("Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
cat("Final Mean-Reversion Strategy Return: ", round(final_strategy_return * 100, 2), "%\n")

# Create a ggplot of cumulative returns
ggplot(sp, aes(x = date)) +
  geom_line(aes(y = cumulative_market_return, color = "Buy-and-Hold"), size = 0.4) +
  geom_line(aes(y = cumulative_strategy_return, color = "MR"), size = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black", size = 0.1) +
  scale_color_manual(values = c("Buy-and-Hold" = "darkred", "MR" = "forestgreen")) +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.margin = margin(t = -15))
ggsave("s&p mr.pdf", dpi = 320, width = 4, height = 3)
dev.off()

# Plot the cumulative returns using base R
plot(sp$date, sp$cumulative_market_return, type = "l", col = "steelblue3", lwd = 2,
     xlab = "Date", ylab = "Cumulative Return", main = "Cumulative Returns: Mean-Reversion Strategy vs Buy-and-Hold")
lines(sp$date, sp$cumulative_strategy_return, col = "darkorange", lwd = 2)
legend("topleft", legend = c("Buy-and-Hold", "Mean-Reversion Strategy"),
       col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)

dev.off()


# Loop through each period dataset in sp_list and process them
for (i in 1:length(sp_list)) {
  
  # Extract current period dataset
  sp_period <- sp_list[[i]]
  
  # Calculate rolling mean and standard deviation
  sp_period <- sp_period %>%
    mutate(rolling_mean = rollmean(adjusted, window, fill = NA, align = "right"),
           rolling_sd = rollapply(adjusted, width = window, FUN = sd, fill = NA, align = "right")) %>%
    na.omit()  # Ensure no missing values
  
  # Calculate Z-score
  sp_period <- sp_period %>%
    mutate(z_score = (adjusted - rolling_mean) / rolling_sd) %>%
    na.omit()  # Ensure no missing values
  
  # Generate buy (Z-score < -1.5) and sell (Z-score > 1.5) signals
  sp_period <- sp_period %>%
    mutate(signal = case_when(
      z_score < -1.5 ~ 1,  # Buy when Z-score < -1.5 (oversold)
      z_score > 1.5  ~ -1, # Sell when Z-score > 1.5 (overbought)
      TRUE ~ 0       # Hold otherwise
    )) %>%
    na.omit()  # Ensure no missing values
  
  # Shift signal by one day to represent actual trading action
  sp_period <- sp_period %>%
    mutate(trade_signal = lag(signal)) %>%
    na.omit()
  
  # Calculate strategy returns based on signals
  sp_period <- sp_period %>%
    mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                    ifelse(trade_signal == -1, -return, 0)))
  
  # Compute cumulative returns for both buy-and-hold and mean-reversion strategy
  sp_period <- sp_period %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1,
           cumulative_strategy_return = cumprod(1 + strategy_return) - 1)
  
  # Final return calculations
  final_market_return <- last(sp_period$cumulative_market_return)
  final_strategy_return <- last(sp_period$cumulative_strategy_return)
  
  # Print the final returns for the current period
  cat("Period:", periods[i], "\n")
  cat("Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
  cat("Final Mean-Reversion Strategy Return: ", round(final_strategy_return * 100, 2), "%\n\n")
  
  # Plot the cumulative returns for the current period
  plot(sp_period$date, sp_period$cumulative_market_return, type = "l", col = "steelblue3", lwd = 2, ylim = c(-1, 2),
       xlab = "Date", ylab = "Cumulative Return", main = paste("Cumulative Returns: Mean-Reversion Strategy vs Buy-and-Hold (", periods[i], ")", sep=""))
  lines(sp_period$date, sp_period$cumulative_strategy_return, col = "darkorange", lwd = 2)
  legend("topleft", legend = c("Buy-and-Hold", "Mean-Reversion Strategy"),
         col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)
}

dev.off()

rm(list = ls())
