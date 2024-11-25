# Load necessary libraries
library(dplyr)
library(tidyquant)
library(tidyverse)

# Data
sp <- tq_get("^GSPC", from = "1980-01-01", to = "2020-01-01")

sp <- sp %>% 
  mutate(return = adjusted / lag(adjusted) - 1,
                    log_return = log(adjusted / lag(adjusted))) %>%
  na.omit()

sp <- sp %>% 
  mutate(rsi = RSI(adjusted, n = 14))

# Define RSI threshold sets
thresholds <- list(c(30, 70), c(20, 80))
threshold_labels <- c("(30, 70)", "(20, 80)")

# Initialize data frame for cumulative returns
strategy_returns <- sp %>% select(date, return) %>%
  mutate(cumulative_market_return = cumprod(1 + return) - 1)

# Loop through thresholds
for (i in seq_along(thresholds)) {
  lower_thresh <- thresholds[[i]][1]
  upper_thresh <- thresholds[[i]][2]
  label <- threshold_labels[i]
  
  sp_temp <- sp %>% mutate(signal = case_when(
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
    sp_temp %>% select(date, cumulative_strategy_return),
    by = "date"
  ) %>% rename(!!paste0("Strategy_", label) := cumulative_strategy_return)
  
  final_market_return <- last(sp_temp$cumulative_market_return)
  final_strategy_return <- last(sp_temp$cumulative_strategy_return)
  
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
  scale_color_manual(values = c("B&H" = "darkred",
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
ggsave("s&p rsi F.pdf", plot, dpi = 320, width = 4, height = 3)
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

# Loop through periods
for (j in seq_along(sp_list)) {
  sp_sub <- sp_list[[j]]
  period <- periods[j]
  cat("Period:", period, "\n")
  
  # Initialize data frame for cumulative returns
  strategy_returns <- sp_sub %>% select(date, return) %>%
    mutate(cumulative_market_return = cumprod(1 + return) - 1)
  
  # Loop through thresholds
  for (i in seq_along(thresholds)) {
    lower_thresh <- thresholds[[i]][1]
    upper_thresh <- thresholds[[i]][2]
    label <- threshold_labels[i]
    
    sp_temp <- sp_sub %>% mutate(signal = case_when(
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
    
    final_market_return <- last(sp_temp$cumulative_market_return)
    final_strategy_return <- last(sp_temp$cumulative_strategy_return)
    
    cat("Final B&H Return: ", round(final_market_return * 100, 2), "%\n")
    cat(sprintf("Final RSI Strategy (%s) Return: ", label), round(final_strategy_return * 100, 2), "%\n\n")
  }
}

dev.off()

rm(list = ls())

