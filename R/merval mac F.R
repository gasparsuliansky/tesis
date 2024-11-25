# Load necessary libraries
library(dplyr)
library(tidyquant)
library(tidyverse)
library(gridExtra)

# Data

# Get MERVAL data from 1980-01-01 to 2020-01-01
merval <- tq_get("^MERV", from = "1980-01-01", to = "2020-01-01")

# Compute daily log returns
merval <- merval %>%
  mutate(return = adjusted / lag(adjusted) - 1,
         log_return = c(NA, diff(log(adjusted)))) %>%
  na.omit()

# Define moving average window combinations
windows <- list(c(10, 50), c(20, 100), c(50, 200))
window_labels <- c("(10, 50)", "(20, 100)", "(50, 200)")  # Labels for the strategies

# Initialize a data frame to store cumulative returns
strategy_returns <- merval %>%
  select(date, return) %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1)

# Loop through each combination of windows
for (i in seq_along(windows)) {
  short_window <- windows[[i]][1]
  long_window <- windows[[i]][2]
  label <- window_labels[i]
  
  # Calculate moving averages and signals
  merval_temp <- merval %>%
    mutate(short_ma = rollmean(adjusted, short_window, fill = NA, align = "right"),
           long_ma = rollmean(adjusted, long_window, fill = NA, align = "right")) %>%
    na.omit() %>%
    mutate(signal = case_when(
      short_ma > long_ma ~ 1,  
      short_ma < long_ma ~ -1, 
      TRUE ~ 0  # Hold otherwise
    )) %>%
    mutate(trade_signal = lag(signal)) %>%
    na.omit() %>%
    mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                    ifelse(trade_signal == -1, -return, 0))) %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1,  # Calculate to match dates
           cumulative_strategy_return = cumprod(1 + strategy_return) - 1)
  
  # Add the cumulative strategy return to the main data frame
  strategy_returns <- strategy_returns %>%
    left_join(merval_temp %>% select(date, cumulative_strategy_return), by = "date") %>%
    rename(!!paste0("Strategy_", label) := cumulative_strategy_return)
  
  # Final return calculations
  final_market_return <- last(merval_temp$cumulative_market_return)
  final_strategy_return <- last(merval_temp$cumulative_strategy_return)
  
  # Print final returns for the window combination
  cat(sprintf("For %d-%d Moving Averages:\n", short_window, long_window))
  cat("  Final B&H Return: ", round((final_market_return) * 100, 2), "%\n")
  cat("  Final Strategy Return: ", round((final_strategy_return) * 100, 2), "%\n\n")
}

# Prepare data for plotting
plot_data <- strategy_returns %>%
  select(date, cumulative_market_return, starts_with("Strategy_")) %>%
  pivot_longer(cols = -date, names_to = "Strategy", values_to = "Cumulative_Return")

# Clean up Strategy names
plot_data$Strategy <- gsub("Strategy_", "", plot_data$Strategy)
plot_data$Strategy <- ifelse(plot_data$Strategy == "cumulative_market_return", "B&H", plot_data$Strategy)

# Create the plot, mapping both linetype and color to Strategy
plot <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, linetype = Strategy, color = Strategy)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c("B&H" = "darkblue",
                                "(10, 50)" = "darkgreen",
                                "(20, 100)" = "darkgreen",
                                "(50, 200)" = "darkgreen")) +
  scale_linetype_manual(values = c("B&H" = "solid",
                                   "(10, 50)" = "dashed",
                                   "(20, 100)" = "dotted",
                                   "(50, 200)" = "dotdash")) +
  ggthemes::theme_base() +
  labs(x = "", y = "", linetype = "Estrategia", color = "Estrategia") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank()) +
  theme(legend.position = "bottom", legend.margin = margin(t = -15), legend.title = element_blank(), legend.text = element_text(margin = margin(r = 2, unit = "pt")))

# Display the plot
print(plot)

# Save the plot to a PDF file
ggsave("merval mac F.pdf", plot, dpi = 320, width = 4, height = 3)


# Filter the data into specific periods
merval_1 <- filter(merval, date < as.Date("2000-01-01"))
merval_2 <- filter(merval, date >= as.Date("2000-01-01") & date < as.Date("2005-01-01"))
merval_3 <- filter(merval, date >= as.Date("2005-01-01") & date < as.Date("2010-01-01"))
merval_4 <- filter(merval, date >= as.Date("2010-01-01") & date < as.Date("2015-01-01"))
merval_5 <- filter(merval, date >= as.Date("2015-01-01") & date < as.Date("2020-01-01"))

# Combine the data into a list and define the periods
merval_list <- list(merval_1, merval_2, merval_3, merval_4, merval_5)
periods <- c("1996-2000", "2000-2005", "2005-2010", "2010-2015", "2015-2020")

# Loop through each period and each combination of windows
for (j in seq_along(merval_list)) {
  merval_sub <- merval_list[[j]]
  period <- periods[j]
  
  cat(sprintf("Period: %s\n", period))
  
  for (i in seq_along(windows)) {
    short_window <- windows[[i]][1]
    long_window <- windows[[i]][2]
    
    # Calculate moving averages and signals
    merval_temp <- merval_sub %>%
      mutate(short_ma = rollmean(adjusted, short_window, fill = NA, align = "right"),
             long_ma = rollmean(adjusted, long_window, fill = NA, align = "right")) %>%
      na.omit() %>%
      mutate(signal = case_when(
        short_ma > long_ma ~ 1,  
        short_ma < long_ma ~ -1, 
        TRUE ~ 0  # Hold otherwise
      )) %>%
      mutate(trade_signal = lag(signal)) %>%
      na.omit() %>%
      mutate(strategy_return = ifelse(trade_signal == 1, return, 
                                      ifelse(trade_signal == -1, -return, 0))) %>%
      mutate(cumulative_market_return = cumprod(1 + return) - 1,
             cumulative_strategy_return = cumprod(1 + strategy_return) - 1)
    
    # Final return calculations
    final_market_return <- last(merval_temp$cumulative_market_return)
    final_strategy_return <- last(merval_temp$cumulative_strategy_return)
    
    # Print final returns for the window combination
    cat(sprintf("  For %d-%d Moving Averages:\n", short_window, long_window))
    cat("    Final B&H Return: ", round(final_market_return * 100, 2), "%\n")
    cat("    Final Strategy Return: ", round(final_strategy_return * 100, 2), "%\n\n")
  }
}

dev.off()

rm(list = ls())
