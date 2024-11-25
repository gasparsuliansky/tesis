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


# Moving Average Crossover 

# Set parameters for short-term and long-term moving averages
short_window <- 50
long_window <- 200

# Calculate moving averages
sp <- sp %>%
  mutate(short_ma = rollmean(adjusted, short_window, fill = NA, align = "right"),
         long_ma = rollmean(adjusted, long_window, fill = NA, align = "right")) %>%
  na.omit()

# Create a signal column: 1 for buy, -1 for sell
sp <- sp %>%
  mutate(signal = case_when(
    short_ma > long_ma ~ 1,  
    short_ma < long_ma ~ -1, 
    TRUE ~ 0       # Hold otherwise
  ))

# Shift signal column by one row to represent the trading action
sp <- sp %>%
  mutate(trade_signal = lag(signal)) %>%
  na.omit()

# Calculate strategy returns based on signals
sp <- sp %>%
  mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                  ifelse(trade_signal == -1, -return, 0)))

# Compute cumulative returns for both buy-and-hold and strategy
sp <- sp %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1,
         cumulative_strategy_return = cumprod(1 + strategy_return) - 1)

# Final return calculations
final_market_return <- last(sp$cumulative_market_return)
final_strategy_return <- last(sp$cumulative_strategy_return)

# Print the final returns
cat("Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
cat("Final Strategy Return: ", round(final_strategy_return * 100, 2), "%\n")

# Create a ggplot of cumulative returns
ggplot(sp, aes(x = date)) +
  geom_line(aes(y = cumulative_market_return, color = "Buy-and-Hold"), linewidth = 0.4) +
  geom_line(aes(y = cumulative_strategy_return, color = "MAC"), linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black", linewidth = 0.1) +
  scale_color_manual(values = c("Buy-and-Hold" = "darkred", "MAC" = "forestgreen")) +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.margin = margin(t = -15))
ggsave("s&p mac.pdf", dpi = 320, width = 4, height = 3)
dev.off()

# Plot the cumulative returns using base R
plot(sp$date, sp$cumulative_market_return, type = "l", col = "steelblue3", lwd = 2,
     xlab = "Date", ylab = "Cumulative Return", main = "Cumulative Returns: Strategy vs Buy-and-Hold")
lines(sp$date, sp$cumulative_strategy_return, col = "darkorange", lwd = 2)
legend("topleft", legend = c("Buy-and-Hold", "MAC Strategy"),
       col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)

dev.off()

# Loop through each period dataset in sp_list and process them
for (i in 1:length(sp_list)) {
  
  # Extract current period dataset
  sp_period <- sp_list[[i]]
  
  # Calculate moving averages
  sp_period <- sp_period %>%
    mutate(short_ma = rollmean(adjusted, short_window, fill = NA, align = "right"),
           long_ma = rollmean(adjusted, long_window, fill = NA, align = "right")) %>%
    na.omit()
  
  # Create a signal column: 1 for buy, -1 for sell
  sp_period <- sp_period %>%
    mutate(signal = case_when(
      short_ma > long_ma ~ 1,  
      short_ma < long_ma ~ -1, 
      TRUE ~ 0  # Hold otherwise
    ))
  
  # Shift signal column by one row to represent the trading action
  sp_period <- sp_period %>%
    mutate(trade_signal = lag(signal)) %>%
    na.omit()
  
  # Calculate strategy returns based on signals
  sp_period <- sp_period %>%
    mutate(strategy_return = case_when(
      trade_signal == 1 ~ return,           # Buy when signal is 1
      trade_signal == -1 ~ -return,         # Short when signal is -1
      TRUE ~ 0                              # Hold when signal is 0
    ))
  
  # Compute cumulative returns for both buy-and-hold and strategy
  sp_period <- sp_period %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1,
           cumulative_strategy_return = cumprod(1 + strategy_return) - 1)
  
  # Final return calculations (no need to subtract 1 here)
  final_market_return <- last(sp_period$cumulative_market_return)
  final_strategy_return <- last(sp_period$cumulative_strategy_return)
  
  # Print the final returns for the current period
  cat("Period:", periods[i], "\n")
  cat("Final Buy-and-Hold Return: ", round(final_market_return * 100, 2), "%\n")
  cat("Final Strategy Return: ", round(final_strategy_return * 100, 2), "%\n\n")
  
  # Plot the cumulative returns for the current period
  plot(sp_period$date, sp_period$cumulative_market_return, type = "l", col = "steelblue3", lwd = 2, ylim = c(-1, 2),
       xlab = "Date", ylab = "Cumulative Return", main = paste("Cumulative Returns: Strategy vs Buy-and-Hold (", periods[i], ")", sep=""))
  lines(sp_period$date, sp_period$cumulative_strategy_return, col = "darkorange", lwd = 2)
  legend("topleft", legend = c("Buy-and-Hold", "MAC Strategy"), col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)
}

dev.off()

rm(list = ls())
