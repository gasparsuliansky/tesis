# Load necessary libraries
library(dplyr)
library(fracdiff)
library(nortest)
library(pracma)
library(tidyquant)
library(tidyverse)
library(tseries)
library(vrtest)


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


# Autocorrelation

# Calculate the autocorrelation function up to lag 10
acf_values <- acf(merval$log_return, lag.max = 10, plot = FALSE)  # Ensure plot = FALSE to get values without plotting

# Convert to a data frame for ggplot
acf_df <- data.frame(lag = acf_values$lag, acf = acf_values$acf)

# Calculate the confidence interval for white noise
conf_level <- 1.96 / sqrt(length(merval$log_return))

# Create ACF plot using ggplot2
ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_line(linetype = "dashed", linewidth = 0.3, col = "darkblue") +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.025) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  geom_hline(yintercept = c(-conf_level, conf_level), linetype = "dashed", color = "black", size = 0.25) +
  ggthemes::theme_base() +
  labs(x = "Lags", y = "") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), labels = as.integer) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12), plot.background = element_blank())
ggsave("merval acf plot.pdf", dpi = 320, width = 4, height = 3)

# Compute significance
acf_values <- acf(merval$log_return, lag.max = 10, plot = FALSE)$acf[-1]

# Determine the sample size
N <- length(merval$log_return)

# Calculate thresholds for 10%, 5%, and 1% significance levels
z_10_percent <- qnorm(1 - 0.1 / 2) / sqrt(N)
z_5_percent <- qnorm(1 - 0.05 / 2) / sqrt(N)
z_1_percent <- qnorm(1 - 0.01 / 2) / sqrt(N)

# Check significance level for each lag
significance <- sapply(acf_values, function(x) {
  if (abs(x) > z_1_percent) {
    return("Significant at 1%")
  } else if (abs(x) > z_5_percent) {
    return("Significant at 5%")
  } else if (abs(x) > z_10_percent) {
    return("Significant at 10%")
  } else {
    return("Not significant")
  }
})

# Print the results
results <- data.frame(lag = 1:10, acf = acf_values, signficance = significance)
print(results)

# H0: The residuals are independently distributed (no autocorrelation).
# H1: The residuals exhibit autocorrelation.

box_pierce <- Box.test(merval$log_return, lag = 10, type = "Box-Pierce")
box_pierce
ifelse(box_pierce$p.value < 0.05, paste("H1"), paste("H0"))

ljung_box <- Box.test(merval$log_return, lag = 10, type = "Ljung-Box")
ljung_box
ifelse(ljung_box$p.value < 0.05, paste("H1"), paste("H0"))

dev.off()

for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the current dataset for the period
  period <- periods[i]  # Get the current period
  
  # Calculate the autocorrelation function up to lag 10
  acf_values <- acf(merval_period$log_return, plot = FALSE, lag.max = 10)$acf[-1]  # Exclude lag 0
  
  # Determine the sample size
  N <- length(merval_period$log_return)
  
  # Calculate thresholds for 10%, 5%, and 1% significance levels
  z_10_percent <- qnorm(1 - 0.1 / 2) / sqrt(N)
  z_5_percent <- qnorm(1 - 0.05 / 2) / sqrt(N)
  z_1_percent <- qnorm(1 - 0.01 / 2) / sqrt(N)
  
  # Check significance level for each lag
  significance <- sapply(acf_values, function(x) {
    if (abs(x) > z_1_percent) {
      return("Significant at 1%")
    } else if (abs(x) > z_5_percent) {
      return("Significant at 5%")
    } else if (abs(x) > z_10_percent) {
      return("Significant at 10%")
    } else {
      return("Not significant")
    }
  })
  
  # Create a data frame with lag, ACF value, and significance level
  results <- data.frame(
    Lag = 1:10,
    ACF = acf_values,
    Significance = significance
  )
  
  # Print the results for autocorrelation up to lag 10
  print(paste("Period:", period))
  print(results)
  
  # Box-Pierce Test
  box_pierce <- Box.test(merval_period$log_return, lag = 10, type = "Box-Pierce")
  print(paste("Period:", period, "Box-Pierce p-value:", box_pierce$p.value, "Box-Pierce statistic", box_pierce$statistic))
  hypothesis_bp <- ifelse(box_pierce$p.value < 0.05, "Reject H0", "Fail to reject H0")
  print(paste("Box-Pierce result:", hypothesis_bp))
  
  # Ljung-Box Test
  ljung_box <- Box.test(merval_period$log_return, lag = 10, type = "Ljung-Box")
  print(paste("Period:", period, "Ljung-Box p-value:", ljung_box$p.value, "Ljung-Box statistic", ljung_box$statistic))
  hypothesis_lb <- ifelse(ljung_box$p.value < 0.05, "Reject H0", "Fail to reject H0")
  print(paste("Ljung-Box result:", hypothesis_lb))
  
  # Print a separator for better readability in the console
  print("-----------------------------------------------------")
}


# Normality

qqnorm(merval$log_return, main = "QQ Plot of S&P 500 Returns", pch = 19, col = alpha("darkblue", 0.25))
qqline(merval$log_return, lty = 2)

# Create the plot
ggplot(merval, aes(sample = log_return)) +
  stat_qq(color = "darkblue", alpha = 0.25, shape = 19) +
  stat_qq_line(linetype = "dashed") +
  labs(x = "Teórico", y = "Observado") +
  ggthemes::theme_base() +
  labs(x = "Teórico", y = "Observado") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme(text = element_text(family = "Times", size = 12),
        plot.background = element_blank())
ggsave("s&p qq plot.pdf", dpi = 320, width = 4, height = 3)
dev.off()

# H0: The data follows a normal distribution.
# H1: The data does not follow a normal distribution.

anderson_darling <- ad.test(merval$log_return)
anderson_darling
ifelse(anderson_darling$p.value < 0.05, paste("H1"), paste("H0"))

jarque_bera <- jarque.bera.test(merval$log_return)
print(paste("Jarque-Bera statistic:", round(jarque_bera$statistic, 4), 
            "p-value:", format(jarque_bera$p.value, scientific = TRUE, digits = 3)))

dev.off()

# Loop over each dataset to create QQ plots and perform normality tests
for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the dataset for the current period
  period <- periods[i]       # Get the label for the current period
  
  # QQ Plot
  qqnorm(merval_period$log_return, main = paste("QQ Plot of S&P 500 Returns -", period), pch = 19, col = alpha("darkblue", 0.25))
  qqline(merval_period$log_return, lty = 2)
  
  # Anderson-Darling Test
  anderson_darling <- ad.test(merval_period$log_return)
  print(paste("Period:", period, 
              "Anderson-Darling statistic:", round(anderson_darling$statistic, 4), 
              "p-value:", format(anderson_darling$p.value, scientific = TRUE, digits = 3)))
  
  # Jarque-Bera Test
  jarque_bera <- jarque.bera.test(merval_period$log_return)
  print(paste("Period:", period, 
              "Jarque-Bera statistic:", round(jarque_bera$statistic, 4), 
              "p-value:", format(jarque_bera$p.value, scientific = TRUE, digits = 3)))
  
  # Shapiro-Wilk Test
  shapiro_wilk <- shapiro.test(merval_period$log_return)
  print(paste("Period:", period, 
              "Shapiro-Wilk statistic:", round(shapiro_wilk$statistic, 4), 
              "p-value:", format(shapiro_wilk$p.value, scientific = TRUE, digits = 3)))
  
  # Print a separator for readability
  print("-----------------------------------------------------")
}

dev.off()


# Unit Root

# H0: The time series has a unit root (non-stationary).
# H1: The time series is stationary (no unit root).

augmented_dickey_fuller <- adf.test(merval$log_return)
augmented_dickey_fuller
ifelse(augmented_dickey_fuller$p.value < 0.05, paste("H1"), paste("H0"))

# Loop over each dataset and perform Augmented Dickey-Fuller test
for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the current dataset for the period
  period <- periods[i]  # Get the current period
  
  # Augmented Dickey-Fuller Test
  augmented_dickey_fuller <- adf.test(merval_period$log_return)
  print(paste("Period:", period, "Augmented Dickey-Fuller statistic:", augmented_dickey_fuller$statistic, "Augmented Dickey-Fuller p-value:", augmented_dickey_fuller$p.value, "Augmented Dickey-Fuller lags:", augmented_dickey_fuller$parameter))
  
  # Hypothesis Testing: H0 (non-stationary) vs H1 (stationary)
  hypothesis_adf <- ifelse(augmented_dickey_fuller$p.value < 0.05, "Reject H0 (Stationary)", "Fail to reject H0 (Non-Stationary)")
  print(paste("ADF Test result:", hypothesis_adf))
  
  # Print a separator for better readability in the console
  print("-----------------------------------------------------")
}


# Runs Test

# H0: The sequence of price changes is random.
# H1: The sequence of price changes is not random (presence of a pattern).

merval <- merval %>%
  mutate(sign_return = case_when(
    log_return > 0 ~ 1,
    log_return == 0 ~ 0,
    log_return < 0 ~ -1
  )) %>%
  na.omit()

pos_count <- sum(merval$sign_return == 1)
zero_count <- sum(merval$sign_return == 0)
neg_count <- sum(merval$sign_return == -1)

runs <- sum(diff(merval$sign_return) != 0) + 1
n <- length(merval$sign_return)
expected_runs <- (2 * pos_count * neg_count + 2 * pos_count * zero_count + 2 * neg_count * zero_count) / n + 1
std_runs <- sqrt((2 * pos_count * neg_count * (2 * pos_count * neg_count - n)) / (n^2 * (n - 1)))
z_score <- (runs - expected_runs) / std_runs

cat("Number of negative returns:", neg_count, "\n")
cat("Number of zero returns:", zero_count, "\n")
cat("Number of positive returns:", pos_count, "\n")
cat("Number of runs:", runs, "\n")
cat("Expected number of runs:", expected_runs, "\n")
cat("Standard deviation of runs:", std_runs, "\n")
cat("Z-score:", z_score, "\n")
p_value_runs <- 2 * pnorm(-abs(z_score))
cat("P-value:", p_value_runs, "\n")
ifelse(p_value_runs < 0.05, paste("H1"), paste("H0"))

# Loop over each dataset and perform the Runs Test
for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the current dataset for the period
  period <- periods[i]  # Get the current period
  
  # Create a sign return column based on log_return values
  merval_period <- merval_period %>%
    mutate(sign_return = case_when(
      log_return > 0 ~ 1,
      log_return == 0 ~ 0,
      log_return < 0 ~ -1
    )) %>%
    na.omit()
  
  # Count the number of positive, zero, and negative sign returns
  pos_count <- sum(merval_period$sign_return == 1)
  zero_count <- sum(merval_period$sign_return == 0)
  neg_count <- sum(merval_period$sign_return == -1)
  
  # Calculate the number of runs
  runs <- sum(diff(merval_period$sign_return) != 0) + 1
  n <- length(merval_period$sign_return)
  
  # Calculate the expected number of runs and standard deviation of runs
  expected_runs <- (2 * pos_count * neg_count + 2 * pos_count * zero_count + 2 * neg_count * zero_count) / n + 1
  std_runs <- sqrt((2 * pos_count * neg_count * (2 * pos_count * neg_count - n)) / (n^2 * (n - 1)))
  
  # Calculate the Z-score for the runs test
  z_score <- (runs - expected_runs) / std_runs
  
  # Calculate the p-value
  p_value_runs <- 2 * pnorm(-abs(z_score))
  
  # Print the results for each period
  cat("Period:", period, "\n")
  cat("Number of negative returns:", neg_count, "\n")
  cat("Number of zero returns:", zero_count, "\n")
  cat("Number of positive returns:", pos_count, "\n")
  cat("Number of runs:", runs, "\n")
  cat("Expected number of runs:", expected_runs, "\n")
  cat("Standard deviation of runs:", std_runs, "\n")
  cat("Z-score:", z_score, "\n")
  cat("P-value:", p_value_runs, "\n")
  
  # Hypothesis testing: H0 (random sequence) vs H1 (non-random sequence)
  hypothesis_runs <- ifelse(p_value_runs < 0.05, "Reject H0 (Non-random sequence)", "Fail to reject H0 (Random sequence)")
  cat("Runs Test result:", hypothesis_runs, "\n")
  
  # Print a separator for better readability in the console
  cat("-----------------------------------------------------\n")
}


# Variance Ratio

# H0: The variance ratio is equal to 1, indicating that the time series follows a random walk.
# H1: The variance ratio is not equal to 1, suggesting that the time series deviates from a random walk.

# Define the lag periods you want to test
lag_periods <- c(2, 5, 10)

# Perform the Lo-MacKinlay test for the specified lag periods
lo_mac <- Lo.Mac(merval$log_return, kvec = lag_periods)

# Extract M1 and M2 Z statistics
z_stat_m1 <- lo_mac$Stats[, "M1"]
z_stat_m2 <- lo_mac$Stats[, "M2"]

# Calculate p-values for M1 and M2 statistics
p_value_m1 <- 2 * pnorm(-abs(z_stat_m1))
p_value_m2 <- 2 * pnorm(-abs(z_stat_m2))

# Calculate the variance ratios for each lag period based on M1 statistics
n <- length(merval$log_return)
variance_ratios <- 1 + (z_stat_m1 / sqrt(n / lag_periods))

# Display results in a tidy format
cat("Lo-MacKinlay Variance Ratio Test Results:\n")
for (j in seq_along(lag_periods)) {
  cat("Lag period:", lag_periods[j], "\n")
  cat("  Variance Ratio (based on M1):", format(variance_ratios[j], digits = 4), "\n")
  cat("  M1 Z-statistic:", format(z_stat_m1[j], digits = 4), "\n")
  cat("  M1 P-value:", format(p_value_m1[j], digits = 4), "\n")
  cat("  M1 Hypothesis Test Result:", 
      ifelse(p_value_m1[j] < 0.05, "Reject H0 (Not a random walk)", "Fail to reject H0 (Random walk)"), "\n")
  cat("  M2 Z-statistic:", format(z_stat_m2[j], digits = 4), "\n")
  cat("  M2 P-value:", format(p_value_m2[j], digits = 4), "\n")
  cat("  M2 Hypothesis Test Result:", 
      ifelse(p_value_m2[j] < 0.05, "Reject H0 (Not a random walk)", "Fail to reject H0 (Random walk)"), "\n")
  cat("-----------------------------------------------------\n")
}

# Loop over the periods
for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the current dataset for the period
  period <- periods[i]  # Get the current period
  
  cat("Period:", period, "\n")
  
  # Lo-MacKinlay Test
  lo_mac <- Lo.Mac(merval_period$log_return, kvec = c(2, 5, 10))
  z_stat_m1 <- lo_mac$Stats[, "M1"]
  z_stat_m2 <- lo_mac$Stats[, "M2"]
  
  # Calculate p-values for each statistic
  p_value_m1 <- 2 * pnorm(-abs(z_stat_m1))
  p_value_m2 <- 2 * pnorm(-abs(z_stat_m2))
  
  # Calculate the variance ratios for each lag period
  n <- length(merval_period$log_return)
  lag_periods <- c(2, 5, 10)
  variance_ratios <- 1 + (z_stat_m1 / sqrt(n / lag_periods))
  
  # Display results in a tidy format
  cat("Lo-MacKinlay Variance Ratio Test Results:\n")
  for (j in seq_along(lag_periods)) {
    cat("Lag period:", lag_periods[j], "\n")
    cat("  Variance Ratio:", format(variance_ratios[j], digits = 4), "\n")
    cat("  M1 Z-statistic:", format(z_stat_m1[j], digits = 4), "\n")
    cat("  M1 P-value:", format(p_value_m1[j], digits = 4), "\n")
    cat("  M1 Hypothesis Test Result:", 
        ifelse(p_value_m1[j] < 0.05, "Reject H0 (Not a random walk)", "Fail to reject H0 (Random walk)"), "\n")
    cat("  M2 Z-statistic:", format(z_stat_m2[j], digits = 4), "\n")
    cat("  M2 P-value:", format(p_value_m2[j], digits = 4), "\n")
    cat("  M2 Hypothesis Test Result:", 
        ifelse(p_value_m2[j] < 0.05, "Reject H0 (Not a random walk)", "Fail to reject H0 (Random walk)"), "\n")
  }
  
  # Print a separator for better readability in the console
  cat("-----------------------------------------------------\n")
}


# Hurst Exponent

# H0: The time series follows a random walk (Hurst exponent ≈ 0.5).
# H1: The time series does not follow a random walk (Hurst exponent significantly different from 0.5).

# Calculate the Hurst exponent for the actual data
hurst_exp <- hurstexp(merval$log_return, display = F)

# Monte Carlo Simulation to generate a distribution of Hurst exponents under the null hypothesis
n_sim <- 1000
hurst_sim <- numeric(n_sim)
set.seed(123)

for (i in 1:n_sim) {
  simulated_data <- fracdiff.sim(n = length(merval$log_return), d = 0)$series
  hurst_sim[i] <- hurstexp(simulated_data, display = FALSE)$Hrs
}

# Calculate the p-value
p_value_hurst <- mean(abs(hurst_sim - mean(hurst_sim)) >= abs(hurst_exp$Hrs - mean(hurst_sim)))

# Display summary of results
cat("Summary of Hurst Exponent Analysis:\n")
cat("Observed Hurst Exponent (Hrs):", round(hurst_exp$Hrs, 4), "\n")
cat("Mean Simulated Hurst Exponent:", round(mean(hurst_sim), 4), "\n")
cat("Standard Deviation of Simulated Hurst Exponents:", round(sd(hurst_sim), 4), "\n")
cat("P-value from Monte Carlo Simulation:", round(p_value_hurst, 4), "\n\n")

# Define the number of simulations
n_sim <- 1000

# Loop over each dataset and compute the Hurst exponent and perform Monte Carlo simulation
for (i in 1:length(merval_list)) {
  merval_period <- merval_list[[i]]  # Get the current dataset for the period
  period <- periods[i]       # Get the current period
  
  cat("Period:", period, "\n")
  
  # Hurst Exponent Calculation
  hurst_exp <- hurstexp(merval_period$log_return, display = F)
  
  # Monte Carlo Simulation
  hurst_sim <- numeric(n_sim)
  set.seed(123)  # Set seed for reproducibility
  
  for (j in 1:n_sim) {
    simulated_data <- fracdiff.sim(n = length(merval_period$log_return), d = 0)$series
    hurst_sim[j] <- hurstexp(simulated_data, display = FALSE)$Hrs
  }
  
  # Calculate p-value
  p_value_hurst <- mean(abs(hurst_sim - mean(hurst_sim)) >= abs(hurst_exp$Hrs - mean(hurst_sim)))
  
  # Display results
  cat("Hurst Exponent:", round(hurst_exp$Hrs, 4), "\n")
  cat("Mean Simulated Hurst Exponent:", round(mean(hurst_sim), 4), "\n")
  cat("Standard Deviation of Simulated Hurst Exponents:", round(sd(hurst_sim), 4), "\n")
  cat("P-value from Monte Carlo Simulation:", round(p_value_hurst, 4), "\n\n")
  
  # Print a separator for readability
  cat("-----------------------------------------------------\n")
}

dev.off()

rm(list = ls())
