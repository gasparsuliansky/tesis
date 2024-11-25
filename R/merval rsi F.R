# Load necessary libraries
library(dplyr)
library(tidyquant)
library(tidyverse)

# Data
merval <- tq_get("^MERV", from = "1980-01-01", to = "2020-01-01")

merval <- merval %>%
  mutate(return = adjusted / lag(adjusted) - 1,
         log_return = c(NA, diff(log(adjusted)))) %>%
  na.omit()

merval <- merval %>% 
  mutate(rsi = RSI(adjusted, n = 14))

# Define RSI threshold sets
thresholds <- list(c(30, 70), c(20, 80))
threshold_labels <- c("(30, 70)", "(20, 80)")

# Initialize data frame for cumulative returns
strategy_returns <- merval %>% select(date, return) %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1)

# Loop through thresholds
for (i in seq_along(thresholds)) {
  lower_thresh <- thresholds[[i]][1]
  upper_thresh <- thresholds[[i]][2]
  label <- threshold_labels[i]
  
  merval_temp <- merval %>% mutate(signal = case_when(
    rsi < lower_thresh ~ 1,
    rsi > upper_thresh ~ -1,
    TRUE ~ 0
  )) %>% mutate(trade_signal = lag(signal)) %>%
    na.omit() %>% mutate(strategy_return = ifelse(
      trade_signal == 1, return,
      ifelse(trade_signal == -1, -return, 0)
    )) %>% mutate(
      cumulative_strategy_return = cumprod(1 + strategy_return) - 1,
      cumulative_market_return = cumprod(1 + return) - 1
    )
  
  strategy_returns <- strategy_returns %>% left_join(
    merval_temp %>% select(date, cumulative_strategy_return),
    by = "date"
  ) %>% rename(!!paste0("Strategy_", label) := cumulative_strategy_return)
  
  final_market_return <- last(merval_temp$cumulative_market_return)
  final_strategy_return <- last(merval_temp$cumulative_strategy_return)
  
  cat("Final B&H Return: ", round(final_market_return * 100, 2), "%\n")
  cat(sprintf("Final RSI Strategy (%s) Return: ", label), round(final_strategy_return * 100, 2), "%\n\n")
}

# Prepare data for plotting
plot_data <- strategy_returns %>% select(date, cumulative_market_return, starts_with("Strategy_")) %>%
  pivot_longer(cols = -date, names_to = "Strategy", values_to = "Cumulative_Return")

# Clean up Strategy names
plot_data$Strategy <- gsub("Strategy_", "", plot_data$Strategy)
plot_data$Strategy <- ifelse(plot_data$Strategy == "cumulative_market_return", "B&H", plot_data$Strategy)

# Create the plot
plot <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Strategy, linetype = Strategy)) +
  geom_line(size = 0.3) +
  scale_color_manual(values = c("B&H" = "darkblue",
                                "(30, 70)" = "darkgreen",
                                "(20, 80)" = "darkgreen")) +
  scale_linetype_manual(values = c("B&H" = "solid",
                                   "(30, 70)" = "dashed",
                                   "(20, 80)" = "dotted")) +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -15))

print(plot)

# Save the plot
ggsave("merval rsi F.pdf", plot, dpi = 320, width = 4, height = 3)
dev.off()


# Filter the data into specific periods
merval_1 <- filter(merval, date < as.Date("2000-01-01"))
merval_2 <- filter(merval, date >= as.Date("2000-01-01") & date < as.Date("2005-01-01"))
merval_3 <- filter(merval, date >= as.Date("2005-01-01") & date < as.Date("2010-01-01"))
merval_4 <- filter(merval, date >= as.Date("2010-01-01") & date < as.Date("2015-01-01"))
merval_5 <- filter(merval, date >= as.Date("2015-01-01") & date < as.Date("2020-01-01"))

# Combine the data into a list and define the periods
merval_list <- list(merval_1, merval_2, merval_3, merval_4, merval_5)
periods <- c("1996-2000", "2000-2005", "2005-2010", "2010-2015", "2015-2020")

# Loop through periods
for (j in seq_along(merval_list)) {
  merval_sub <- merval_list[[j]]
  period <- periods[j]
  cat("Period:", period, "\n")
  
  # Initialize data frame for cumulative returns
  strategy_returns <- merval_sub %>% select(date, return) %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1)
  
  # Loop through thresholds
  for (i in seq_along(thresholds)) {
    lower_thresh <- thresholds[[i]][1]
    upper_thresh <- thresholds[[i]][2]
    label <- threshold_labels[i]
    
    merval_temp <- merval_sub %>% mutate(signal = case_when(
      rsi < lower_thresh ~ 1,
      rsi > upper_thresh ~ -1,
      TRUE ~ 0
    )) %>% mutate(trade_signal = lag(signal)) %>%
      na.omit() %>% mutate(strategy_return = ifelse(
        trade_signal == 1, return,
        ifelse(trade_signal == -1, -return, 0)
      )) %>% mutate(
        cumulative_strategy_return = cumprod(1 + strategy_return) - 1,
        cumulative_market_return = cumprod(1 + return) - 1
      )
    
    final_market_return <- last(merval_temp$cumulative_market_return)
    final_strategy_return <- last(merval_temp$cumulative_strategy_return)
    
    cat("Final B&H Return: ", round(final_market_return * 100, 2), "%\n")
    cat(sprintf("Final RSI Strategy (%s) Return: ", label), round(final_strategy_return * 100, 2), "%\n\n")
  }
}

dev.off()

rm(list = ls()
   )