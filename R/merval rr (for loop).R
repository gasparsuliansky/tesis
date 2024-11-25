# Load necessary libraries
library(dplyr)
library(glmnet)
library(tidyquant)
library(zoo)


# Data

# Get MERVAL data from 1980-01-01 to 2020-01-01
merval <- tq_get("^MERV", from = "1980-01-01", to = "2020-01-01")

# Compute daily log returns
merval <- merval %>%
  mutate(log_return = c(NA, diff(log(adjusted)))) %>%
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


set.seed(2001)
block_size <- 21  # Approximately one month of trading days
num_bootstraps <- 500

for (j in 1:length(merval_list)) {
  
  cat("Period:", periods[j], "\n")
  
  # Extract current period dataset
  merval_period <- merval_list[[j]]
  
  # Create up to 25 lagged log returns as predictors
  for (i in 1:25) {
    merval_period[[paste0("lag_", i)]] <- lag(merval_period$log_return, i)
  }
  
  # Create the future (lead) log return as the dependent variable
  merval_period <- merval_period %>%
    mutate(lead_return = lead(log_return)) %>%
    na.omit()
  
  # Prepare predictor matrix x and response vector y
  x <- as.matrix(merval_period[, paste0("lag_", 1:25)])
  y <- merval_period$lead_return
  
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
  cat("Directional Accuracy:", dir_accuracy, "\n")
  cat("MAE:", mae, "\n")
  cat("R-squared:", r_squared, "\n\n")
  
  # Moving Block Bootstrap
  n_train <- length(y_train)
  
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
    mean(bootstrap_results[, 4] <= r_squared)      # For R², lower values are worse
  )
  
  # Print p-values
  cat("P-values:\n")
  cat("RMSE:", p_values[1], "\n")
  cat("Directional Accuracy:", p_values[2], "\n")
  cat("MAE:", p_values[3], "\n")
  cat("R-squared:", p_values[4], "\n\n")
  
  # Plot bootstrap distributions with kernel density estimates
  par(mfrow = c(2, 2))
  
  # RMSE plot
  hist(bootstrap_results[, 1], xlab = "Value", prob = TRUE, col = "darkorange", border = "black",
       main = paste("RMSE (", periods[j], ")", sep = ""))
  lines(density(bootstrap_results[, 1]), col = "black", lwd = 1.5)
  abline(v = rmse, col = "black", lwd = 1.5, lty = 2)
  legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.8, lty = c(1, 2))
  
  # Directional Accuracy plot
  hist(bootstrap_results[, 2], xlab = "Value", prob = TRUE, col = "darkorange", border = "black",
       main = paste("Directional Accuracy (", periods[j], ")", sep = ""))
  lines(density(bootstrap_results[, 2]), col = "black", lwd = 1.5)
  abline(v = dir_accuracy, col = "black", lwd = 1.5, lty = 2)
  legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.8, lty = c(1, 2))
  
  # MAE plot
  hist(bootstrap_results[, 3], xlab = "Value", prob = TRUE, col = "darkorange", border = "black",
       main = paste("MAE (", periods[j], ")", sep = ""))
  lines(density(bootstrap_results[, 3]), col = "black", lwd = 1.5)
  abline(v = mae, col = "black", lwd = 1.5, lty = 2)
  legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.8, lty = c(1, 2))
  
  # R-squared plot
  hist(bootstrap_results[, 4], xlab = "Value", prob = TRUE, col = "darkorange", border = "black",
       main = paste("R-squared (", periods[j], ")", sep = ""))
  lines(density(bootstrap_results[, 4]), col = "black", lwd = 1.5)
  abline(v = r_squared, col = "black", lwd = 1.5, lty = 2)
  legend("topright", legend = c("Kernel Density", "Actual Value"), lwd = 2, cex = 0.8, lty = c(1, 2))
  
  par(mfrow = c(1, 1))
  
  # Trading Strategy Section
  
  # Check if ridge predictions are constant
  if (sd(as.numeric(ridge_predictions)) < 1e-10) {
    cat("Ridge predictions are constant. Using moving average crossover strategy.\n")
    
    # Calculate short-term (5-day) and long-term (20-day) moving averages
    merval_period$MA5 <- rollmean(merval_period$close, k = 5, fill = NA, align = "right")
    merval_period$MA20 <- rollmean(merval_period$close, k = 20, fill = NA, align = "right")
    
    # Generate signals based on moving average crossover
    merval_period$signal <- ifelse(merval_period$MA5 > merval_period$MA20, 1, 
                               ifelse(merval_period$MA5 < merval_period$MA20, -1, 0))
    
    # Use the signals for the test period
    signal <- merval_period$signal[test_indices]
  } else {
    # Use the original Z-score strategy
    ridge_z_score <- scale(as.numeric(ridge_predictions))
    threshold <- 1
    signal <- ifelse(ridge_z_score > threshold, 1, ifelse(ridge_z_score < -threshold, -1, 0))
  }
  
  # Compute simple returns from log returns
  merval_period <- merval_period %>%
    mutate(return = exp(log_return) - 1)
  
  # Compute strategy returns (daily simple returns of S&P 500 multiplied by the signal)
  strategy_returns <- merval_period$return[test_indices] * lag(signal, default = 0)
  
  # Compute cumulative returns for the strategy using simple returns from log returns
  strategy_cumulative_returns <- cumprod(1 + strategy_returns) - 1
  
  # Compute cumulative returns for the market using the simple returns from log returns
  market_cumulative_returns <- cumprod(1 + merval_period$return[test_indices]) - 1
  
  # Compare cumulative returns
  plot(merval_period$date[test_indices], market_cumulative_returns, type = "l", col = "steelblue3", lwd = 2, 
       main = paste("Cumulative Returns: Strategy vs Market (", periods[j], ")", sep = ""), 
       ylab = "Cumulative Return", xlab = "Time")
  lines(merval_period$date[test_indices], strategy_cumulative_returns, col = "darkorange", lwd = 2)
  legend("topleft", legend = c("Market", "Strategy"), col = c("steelblue3", "darkorange"), lty = 1, lwd = 2)
  
  # Extract final cumulative return for the strategy and market
  strategy_final_return <- tail(strategy_cumulative_returns, 1)
  market_final_return <- tail(market_cumulative_returns, 1)
  
  # Print final returns
  cat("Final Return for Strategy:", round(strategy_final_return * 100, 2), "%\n")
  cat("Final Return for Market:", round(market_final_return * 100, 2), "%\n")
  
  # Print a separator for better readability in the console
  print("-----------------------------------------------------")
}

dev.off()

rm(list = ls())
