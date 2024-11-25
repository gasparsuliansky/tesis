# Load necessary libraries
library(dplyr)
library(glmnet)
library(tidyquant)
library(tidyverse)


# Data

# Get S&P 500 data from 1980-01-01 to 2020-01-01
sp <- tq_get("^GSPC", from = "1980-01-01", to = "2020-01-01")

# Compute daily log returns
sp <- sp %>%
  mutate(log_return = log(adjusted / lag(adjusted)))%>%
  na.omit()

# Create up to 25 lagged log returns as predictors
for (i in 1:25) {
  sp[[paste0("lag_", i)]] <- lag(sp$log_return, i)
}

# Create the future (lead) log return as the dependent variable
sp <- sp %>%
  mutate(lead_return = lead(log_return)) %>%
  na.omit()

# Prepare predictor matrix x and response vector y
x <- as.matrix(sp[, paste0("lag_", 1:25)])
y <- sp$lead_return

# Split data into training and test sets (70% train, 30% test)
train_indices <- 1:floor(0.7 * nrow(x))
test_indices <- (floor(0.7 * nrow(x)) + 1):nrow(x)
x_train <- scale(x[train_indices, ])
y_train <- scale(y[train_indices])
x_test <- scale(x[test_indices, ])
y_test <- scale(y[test_indices])


# Ridge Regression

# Fit ridge regression model on training data
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)

# Make predictions on test set
ridge_predictions <- predict(ridge_model, newx = x_test, s = "lambda.min")

# Check summary of predictions
summary(ridge_predictions)

# Calculate performance metrics on test set
rmse <- sqrt(mean((y_test - ridge_predictions)^2))
dir_accuracy <- mean(sign(y_test) == sign(ridge_predictions))
mae <- mean(abs(y_test - ridge_predictions))  # MAE
r_squared <- 1 - sum((y_test - ridge_predictions)^2) / sum((y_test - mean(y_test))^2)  # R2

# Print performance metrics
cat("Performance Metrics on Test Set:\n")
cat("RMSE:", rmse, "\n")
cat("Directional Accuracy:", dir_accuracy, "\n\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")

# Moving Block Bootstrap

set.seed(2001)
n_train <- length(y_train)
block_size <- 21  # Approximately one month of trading days
num_bootstraps <- 500

# Extend bootstrap to include MAE, R², and correlation
bootstrap_results <- matrix(NA, nrow = num_bootstraps, ncol = 4)
colnames(bootstrap_results) <- c("RMSE", "Dir_Accuracy", "MAE", "R_squared")

for (i in 1:num_bootstraps) {
  start_indices <- sample(1:(n_train - block_size + 1), ceiling(n_train / block_size), replace = TRUE)
  bootstrap_indices <- unlist(lapply(start_indices, function(x) x:(x + block_size - 1)))
  bootstrap_indices <- bootstrap_indices[bootstrap_indices <= n_train]  # Ensure indices are within range
  bootstrap_indices <- bootstrap_indices[1:n_train]  # Adjust length if necessary
  
  x_boot <- x_train[bootstrap_indices, ]
  y_boot <- y_train[bootstrap_indices]
  
  # Retrain model on bootstrapped training data
  model_boot <- cv.glmnet(x_boot, y_boot, alpha = 0)
  
  # Evaluate on the original test set
  predictions_boot <- predict(model_boot, newx = x_test, s = "lambda.min")
  
  # Calculate metrics on test set
  bootstrap_results[i, 1] <- sqrt(mean((y_test - predictions_boot)^2))  # RMSE
  bootstrap_results[i, 2] <- mean(sign(y_test) == sign(predictions_boot))  # Directional Accuracy
  bootstrap_results[i, 3] <- mean(abs(y_test - predictions_boot))  # MAE
  bootstrap_results[i, 4] <- 1 - sum((y_test - predictions_boot)^2) / sum((y_test - mean(y_test))^2)  # R²
}

# Calculate p-values
p_values <- c(
  mean(bootstrap_results[, 1] >= rmse),          # For RMSE, higher values are worse
  mean(bootstrap_results[, 2] <= dir_accuracy),  # For directional accuracy, lower values are worse
  mean(bootstrap_results[, 3] >= mae),           # For MAE, higher values are worse
  mean(bootstrap_results[, 4] <= r_squared)     # For R², lower values are worse
)

# Print p-values
cat("P-values:\n")
cat("RMSE:", p_values[1], "\n")
cat("Directional Accuracy:", p_values[2], "\n")
cat("MAE:", p_values[3], "\n")
cat("R-squared:", p_values[4], "\n")

# Convert bootstrap results into a data frame with named columns for easier reference in ggplot
bootstrap_df <- data.frame(
  RMSE = bootstrap_results[, 1],
  DirectionalAccuracy = bootstrap_results[, 2],
  MAE = bootstrap_results[, 3],
  R_squared = bootstrap_results[, 4]
)

# Create each individual plot
p1 <- ggplot(bootstrap_df, aes(x = RMSE)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "darkred", color = "white", size = 0.1) +
  geom_vline(xintercept = rmse, color = "black", linetype = "dashed", size = 0.5) +
  ggthemes::theme_base() +
  labs(x = "RMSE", y = "Densidad") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 11), plot.background = element_blank())

p2 <- ggplot(bootstrap_df, aes(x = DirectionalAccuracy)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "darkred", color = "white", size = 0.1) +
  geom_vline(xintercept = dir_accuracy, color = "black", linetype = "dashed", size = 0.5) +
  ggthemes::theme_base() +
  labs(x = "Precisión direccional", y = "") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 11), plot.background = element_blank())

p3 <- ggplot(bootstrap_df, aes(x = MAE)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "darkred", color = "white", size = 0.1) +
  geom_vline(xintercept = mae, color = "black", linetype = "dashed", size = 0.5) +
  ggthemes::theme_base() +
  labs(x = "MAE", y = "Densidad") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 11), plot.background = element_blank())

p4 <- ggplot(bootstrap_df, aes(x = R_squared)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "darkred", color = "white", size = 0.1) +
  geom_vline(xintercept = r_squared, color = "black", linetype = "dashed", size = 0.5) +
  ggthemes::theme_base() +
  labs(x = expression(R^2), y = "") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 11), plot.background = element_blank())

# Arrange plots in a 2x2 grid
plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave("s&p rr.pdf", plot = plot, device = "pdf", dpi = 320)

# Plot bootstrap distributions with kernel density estimates
par(mfrow = c(2, 2))

# RMSE plot
hist(bootstrap_results[, 1], main = "RMSE", xlab = "Value", prob = TRUE, col = "darkorange", border = "black")
lines(density(bootstrap_results[, 1]), col = "black", lwd = 1.5)
abline(v = rmse, col = "black", lwd = 1.5, lty = 2)
legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.4, lty = c(1, 2))

# Directional Accuracy plot
hist(bootstrap_results[, 2], main = "Directional Accuracy", xlab = "Value", prob = TRUE, col = "darkorange", border = "black")
lines(density(bootstrap_results[, 2]), col = "black", lwd = 1.5)
abline(v = dir_accuracy, col = "black", lwd = 1.5, lty = 2)
legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.4, lty = c(1, 2))

# MAE plot
hist(bootstrap_results[, 3], main = "MAE", xlab = "Value", prob = TRUE, col = "darkorange", border = "black")
lines(density(bootstrap_results[, 3]), col = "black", lwd = 1.5)
abline(v = mae, col = "black", lwd = 1.5, lty = 2)
legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.4, lty = c(1, 2))

# R-squared plot
hist(bootstrap_results[, 4], main = "R-squared", xlab = "Value", prob = TRUE, col = "darkorange", border = "black")
lines(density(bootstrap_results[, 4]), col = "black", lwd = 1.5)
abline(v = r_squared, col = "black", lwd = 1.5, lty = 2)
legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.4, lty = c(1, 2))

dev.off()


# Trading Strategy Section

# Step 1: Compute Z-score for ridge predictions
ridge_z_score <- scale(as.numeric(ridge_predictions))

# Step 2: Define trading signals based on Z-score criterion (threshold of 1 std dev)
threshold <- 1
signal <- ifelse(ridge_z_score > threshold, 1, ifelse(ridge_z_score < -threshold, -1, 0))

# Compute simple returns from log returns
sp <- sp %>%
  mutate(return = exp(log_return) - 1)

# Step 3: Compute strategy returns (daily simple returns of S&P 500 multiplied by the signal)
strategy_returns <- sp$return[test_indices] * lag(signal, default = 0)

# Step 4: Compute cumulative returns for the strategy using simple returns from log returns
strategy_cumulative_returns <- cumprod(1 + strategy_returns) - 1

# Step 5: Compute cumulative returns for the market using the simple returns from log returns
market_cumulative_returns <- cumprod(1 + sp$return[test_indices]) - 1

# Create a ggplot of cumulative returns
ggplot() +
  geom_line(aes(x = sp$date[test_indices], y = market_cumulative_returns, color = "Buy-and-Hold"), linewidth = 0.4) +
  geom_line(aes(x = sp$date[test_indices], y = strategy_cumulative_returns, color = "RR"), linewidth = 0.4) +
  scale_color_manual(values = c("Buy-and-Hold" = "darkred", "RR" = "forestgreen")) +
  ggthemes::theme_base() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::label_percent(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.margin = margin(t = -15))
ggsave("s&p rr cum.pdf", dpi = 320, width = 4, height = 3)
dev.off()

# Compare cumulative returns
plot(sp$date[test_indices], market_cumulative_returns, type = "l", col = "steelblue3", lwd = 2, 
     main = "Cumulative Returns: Strategy vs Market", ylab = "Cumulative Return", xlab = "Time")
lines(sp$date[test_indices], strategy_cumulative_returns, col = "darkorange", lwd = 2)
legend("topleft", legend = c("Market", "Strategy"), col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)

# Extract final cumulative return for the strategy and market
strategy_final_return <- tail(strategy_cumulative_returns, 1)
market_final_return <- tail(market_cumulative_returns, 1)

# Print final returns
cat("Final Return for Strategy:", round(strategy_final_return * 100, 2), "%\n")
cat("Final Return for Market:", round(market_final_return * 100, 2), "%\n")

dev.off()

rm(list = ls())

# In this code, we implemented a trading strategy based on ridge regression predictions and a Z-score criterion. The ridge regression model was first trained to predict future returns using lagged S&P 500 returns. After making predictions on the test set, we computed the Z-score of these predictions to normalize them. A trading signal was generated when the Z-score exceeded a threshold of 1 standard deviation: a value of 1 (buy signal) for positive Z-scores greater than the threshold, and -1 (sell signal) for negative Z-scores below the threshold.
# Using these signals, the strategy’s daily returns were calculated by multiplying the signal with the market’s actual daily returns. We then computed the cumulative returns for both the strategy and the market. Finally, the code generates a plot that compares the cumulative returns of the trading strategy to the cumulative returns of the S&P 500, allowing us to visually assess the performance of the model-driven trading strategy versus simply holding the market.

# Even if predictors are on the same scale, scaling still improves Ridge regression by equalizing feature variance and ensuring numerical stability during optimization. Features with different variances can lead to uneven penalization, affecting the model's ability to assign balanced coefficients. Standardizing the features ensures they contribute equally to the regularization process and helps the optimization algorithm converge more efficiently.
