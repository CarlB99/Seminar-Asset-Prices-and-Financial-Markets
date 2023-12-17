### Libraries ###
library(tidymodels)
library(tidyverse)
library(ggplot2)
library(RSQLite)
library(quadprog)
library(dplyr)
library(fPortfolio)
library(lpSolve)
library(xts) 
library(gridExtra)
library(tseries)
library(R.utils)

### Data preparation ###

# set working directory
setwd("C:/Users/vilhe/OneDrive/Skrivebord/Seminar - Asset Prices and Financial Markets/Seminar kode")

# get tidy_finance data
tidy_finance <- dbConnect(SQLite(), "tidy_finance.sqlite", extended_types = TRUE)

# get monthly CRSP returns
crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect()
dbDisconnect(tidy_finance)

# Load returns
data <- crsp_monthly |>
  dplyr::filter(month >= as.Date("1980-01-01") & month <= as.Date("2019-12-31")) |>
  select(permno, month, ret = ret,) |>
  na.omit()

# Disregard stocks with discontinuous trading history
data <- data |>
  group_by(permno) |>
  mutate(n = n()) |>
  ungroup() |>
  dplyr::filter(n == max(n)) |> select(-n)

#Reducing the data to allow faster testing
unique_stocks <- unique(data$permno)
set.seed(2023)
num_stocks_to_select <- 200
selected_stocks <- sample(unique_stocks, num_stocks_to_select)
test_data <- data |>
  dplyr::filter(permno %in% selected_stocks)
rm(data)
rm(crsp_monthly)


# Variance-covariance matrix, Sigma, and expected return, mu
returns_matrix <- test_data |>
  pivot_wider(
    names_from = permno,
    values_from = ret) |>
  select(-month)

returns_matrix2 <- test_data |>
  pivot_wider(
    names_from = permno,
    values_from = ret)


#VaR and CVaR

# Define the normal distribution parameters
mean <- 0
sd <- 1

# Generate data for the normal curve
x <- seq(-4, 4, length=1000)
y <- dnorm(x, mean, sd)

# Calculate VaR and CVaR at 95% confidence level
var_95 <- qnorm(0.05, mean, sd)
cvar_95 <- mean - sd * dnorm(qnorm(0.05)) / 0.05

# Create the plot
plot_data <- data.frame(x, y)

area_data <- subset(plot_data, x <= var_95)

normal_distribution <- ggplot(plot_data, aes(x, y)) +
  geom_line() +
  geom_area(data = area_data, aes(x, y), fill="blue", alpha=0.3) +
  geom_vline(xintercept = var_95, color="red", linetype="dashed") +
  annotate("label", x = var_95, y = max(plot_data$y), label = paste("VaR 95% =", round(var_95, 2)), 
           hjust = 1.3, vjust = 2, color = "red", fill = "white", label.size = NA) +
  geom_vline(xintercept = cvar_95, color="black", linetype="dashed") +
  annotate("label", x = cvar_95, y = max(plot_data$y), label = paste("CVaR 95% =", round(cvar_95, 2)), 
           hjust = 1, vjust = 4, color = "black", fill = "white", label.size = NA) +
  theme_bw() +  # White background with black lines
  theme(
    panel.border = element_rect(colour = "black", fill=NA),  # Black border around the plot
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.background = element_blank(),  # Remove background (transparent)
    legend.position = "none",  # Hide the legend
    axis.line = element_line(color = "black"),  # Add black axis lines
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + # Set y-axis expansion to start at 0
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) + # Set x-axis expansion to start at 0
  labs(x = "Mean", y = "Standard deviation", title = "Normal distribution with VaR and CVaR")  # Set custom labels and title

normal_distribution


# Plotting data

returns_df <- as.data.frame(returns_matrix2)
returns_df[,1] <- as.Date(returns_df[,1], format = 'your_date_format')

row_means <- rowMeans(returns_df[, -1])
cumulated_row_means <- cumsum(row_means)
row_volatility <- apply(returns_df[, -1], 1, sd)
jarque_bera_values <- jarque.bera.test(row_means)
adf_test_result <- adf.test(row_means, alternative = "stationary")
adf_test_result

# Define the number of bins for the histogram
bin_width <- (max(row_means) - min(row_means)) / 20  # adjust 30 to change the number of bins


# Prepare the data for plotting
data_to_plot <- data.frame(Date = returns_df[,1], Average = row_means, Cumulative = cumulated_row_means, Volatility = row_volatility)


# Create plot for mean log returns
p1 <- ggplot(data_to_plot, aes(x = Date, y = Average)) +
  geom_line() +
  labs(x = "Year", y = "Log return", title = "Mean monthly return") +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),  # Black border around the plot
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.background = element_blank(),  # Remove background (transparent)
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Create plot for cumulative mean log returns
p2 <- ggplot(data_to_plot, aes(x = Date, y = cumulated_row_means)) +
  geom_line() +
  labs(x = "Year", y = "Log return", title = "Cumulative mean return") +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),  # Black border around the plot
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.background = element_blank(),  # Remove background (transparent)
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

p3 <- ggplot(data_to_plot, aes(x = Date, y = Volatility)) +
  geom_line() +
  labs(x = "Year", y = "Volatility", title = "Average volatility") +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),  # Black border around the plot
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.background = element_blank(),  # Remove background (transparent)
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Create the histogram plot
p4 <- ggplot(data.frame(row_means), aes(x = row_means)) +
  geom_histogram(aes(y = ..density.., fill = 'Sample distribution'), binwidth = bin_width, color = 'black', alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(row_means), sd = sd(row_means)), 
                aes(color = "Normal distribution"), size = 1, linetype = "dashed") +
  labs(title = "Distribution of mean returns",
       x = "Mean return", y = "Density") +
  scale_fill_manual(values = c("blue"), labels = c("Sample distribution")) +
  scale_color_manual(values = c("red"), labels = c("Normal distribution")) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = c(0.95, 0.65),  # Place the legend inside the plot at the bottom
    legend.justification = "right",   # Anchor the legend at the bottom right
    legend.title = element_blank(),  # Remove the legend title
    legend.background = element_blank(),  # Transparent legend background
    legend.key = element_blank(),  # Remove the keys background
    legend.text = element_text(size = 5),  # Set smaller text size for the legend
    legend.key.size = unit(0.5, 'lines'),  # Use smaller keys in legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)  # Remove margin around legend
  )

# Combine the plots side by side
grid.arrange(p1, p2, p3, p4, ncol = 2)


# Optimization using CVAR

#Transforming data
permno_portfolio <- as.timeSeries(returns_matrix2)
# Settings
nAssets <- ncol(permno_portfolio)
mvSpec <- portfolioSpec()
setRiskFreeRate(mvSpec) <- 0
setNFrontierPoints(mvSpec) <- 25
setAlpha(mvSpec) <- 0.05
setSolver(mvSpec) <- "solveRquadprog"
setOptimize(mvSpec) <- "maxReturn"
print(mvSpec)
longFrontier <- portfolioFrontier(
  data = permno_portfolio,
  spec = mvSpec,
  constraints = "LongOnly")
print(longFrontier)
frontierPlot(longFrontier, return = "mu", risk = "CVaR", auto = FALSE, pch = 16,
             type = "b", cex = 0.5, col = c("black", "grey"))
#tangencyLines(longFrontier, risk = "Sigma")
#singleAssetPoints(longFrontier, risk = "Sigma")

longSpec <- portfolioSpec()
setRiskFreeRate(longSpec) <- 0
setNFrontierPoints(longSpec) <- 25
setAlpha(longSpec) <- 0.05
setType(longSpec) <- "CVaR"
setSolver(longSpec) <- "solveRglpk.CVAR"
print(longSpec)
CVaR_frontier <- portfolioFrontier(
  data = permno_portfolio,
  spec = longSpec,
  constraints = "LongOnly")
print(CVaR_frontier)
frontierPlot(CVaR_frontier, return ="mu", risk = "CVaR", auto = FALSE, pch = 16,
             type = "b", cex = 0.5, col = c("blue", "red"), add=TRUE)
#tangencyLines(CVaR_frontier, risk="CVaR")

num_stocks_to_select_2 <- 50
selected_stocks_2 <- sample(selected_stocks, num_stocks_to_select_2)
column_indices <- c("month", selected_stocks_2)
selected_returns_matrix <- returns_matrix2[, column_indices, drop = FALSE]
equal_weight <- 1 / num_stocks_to_select_2
weighted_returns <- selected_returns_matrix[, -1] * equal_weight
NaiveInvestor <- rowSums(weighted_returns, na.rm = TRUE)
selected_returns_matrix <- cbind(selected_returns_matrix, NaiveInvestor)
permno_portfolio <- ts(selected_returns_matrix, start = c(1980, 1), frequency = 12)
permno_portfolio <- as.timeSeries(permno_portfolio)
column_headers <- colnames(selected_returns_matrix)[-c(1, ncol(selected_returns_matrix))]
stock_identifiers <- colnames(permno_portfolio)[-c(1, ncol(permno_portfolio))]
backtestSpec <- portfolioSpec()
permnoConstraints <- "LongOnly"
setAlpha(backtestSpec) <- 0.05
setOptimize(backtestSpec) <- "maxReturn"
setType(backtestSpec) <- "CVaR"
setSolver(backtestSpec) <- "solveRglpk.CVAR"
permnoBacktest <- portfolioBacktest()
setWindowsHorizon(permnoBacktest) <- "60m"
permnoFormulaStr <- paste("NaiveInvestor ~", paste(stock_identifiers, collapse = " + "))
permnoFormula <- as.formula(permnoFormulaStr)

Backtest_permno <- portfolioBacktesting(formula = permnoFormula,
                              data = permno_portfolio, spec = backtestSpec, constraints = permnoConstraints,
                              backtest = permnoBacktest, trace = FALSE)

setSmootherLambda(Backtest_permno$backtest) <- "6m"
permnoSmooth <- portfolioSmoothing(object = Backtest_permno, trace = FALSE)
overall_net_performance <- netPerformance(permnoSmooth)
backtestPortfolioPlot(permnoSmooth, labels=FALSE, legend=FALSE, at=NULL, format=NULL)
title(main="CVaR Portfolio vs Benchmark", ylab="Cumulated log return")
legend("bottomright", # Position of the legend
       legend = c("CVaR", "Benchmark"), # Text for the legend
       col = c("red", "blue"), # Colors matching those in the plot
       lty = c(1, 1), # Line types, assuming both are solid lines
       cex = 0.8) # Adjust text size as needed

num_stocks_to_select_2 <- 50
num_combinations <- 100
selected_stocks_combinations <- list()

for (i in 1:num_combinations) {
  selected_stocks_2 <- sample(selected_stocks, num_stocks_to_select_2)
  selected_stocks_combinations[[i]] <- selected_stocks_2
}

# Define the stock ID that you want to remove from combination 5

all_net_performance <- list()

# Loop over each combination to perform backtesting
# List of indices to skip 

for (i in 1:min(50, length(selected_stocks_combinations))) {
  
  start_time <- Sys.time()  # Record the start time
  
  # Select the current combination of stocks
  selected_stocks_2 <- selected_stocks_combinations[[i]]
  column_indices <- c("month", selected_stocks_2)
  
  # Perform the operations for the selected combination
  selected_returns_matrix <- returns_matrix2[, column_indices, drop = FALSE]
  # Calculate equally weighted returns
  equal_weight <- 1 / num_stocks_to_select_2
  weighted_returns <- selected_returns_matrix[, -1] * equal_weight
  naive_investor_returns <- rowSums(weighted_returns, na.rm = TRUE)
  # Add the NaiveInvestor returns to the matrix
  selected_returns_matrix <- cbind(selected_returns_matrix, NaiveInvestor = naive_investor_returns)
  permno_portfolio <- ts(selected_returns_matrix, start = c(1980, 1), frequency = 12)
  permno_portfolio <- as.timeSeries(permno_portfolio)
  
  # Define your portfolioSpec and other parameters
  backtestSpec <- portfolioSpec()
  permnoConstraints <- "LongOnly"
  setAlpha(backtestSpec) <- 0.05
  setOptimize(backtestSpec) <- "maxReturn"
  setType(backtestSpec) <- "CVAR"
  setSolver(backtestSpec) <- "solveRglpk.CVAR"
  permnoBacktest <- portfolioBacktest()
  setWindowsHorizon(permnoBacktest) <- "60m"
  permnoFormulaStr <- paste("NaiveInvestor ~", paste(selected_stocks_2, collapse = " + "))
  permnoFormula <- as.formula(permnoFormulaStr)
  
  # Perform backtesting
  Backtest_permno <- portfolioBacktesting(formula = permnoFormula, data = permno_portfolio, spec = backtestSpec, constraints = permnoConstraints, backtest = permnoBacktest, trace = FALSE)
  
  setSmootherLambda(Backtest_permno$backtest) <- "6m"
  permnoSmooth <- portfolioSmoothing(object = Backtest_permno, trace = FALSE)
  overall_net_performance <- netPerformance(permnoSmooth)
  total_calendar <- overall_net_performance$Calendar
  all_net_performance[[i]] <- total_calendar
  
  # Calculate the time spent
  end_time <- Sys.time()
  time_spent <- end_time - start_time
  cat("Time spent on combination", i, ": ", time_spent, "seconds\n")
}


saveRDS(all_net_performance, "CVAR.rds")


# Function to process each .rds file
process_rds_file <- function(file_name) {
  # Read the .rds file
  list_data <- readRDS(file_name)
  # Filter out NULL values
  list_data <- Filter(Negate(is.null), list_data)  
  
  # Initialize vectors to store the totals
  portfolio_totals <- numeric(length(list_data))
  benchmark_totals <- numeric(length(list_data))
  
  # Loop through the list and extract the Total for Portfolio and Benchmark
  for (i in seq_along(list_data)) {
    if (!is.null(list_data[[i]])) {
      # Assuming the "Total" is the last column in each matrix
      portfolio_totals[i] <- list_data[[i]][1, ncol(list_data[[i]])]
      benchmark_totals[i] <- list_data[[i]][2, ncol(list_data[[i]])]
    }
  }
  
  # Calculate mean values for portfolio and benchmark
  mean_portfolio <- mean(portfolio_totals, na.rm = TRUE)
  mean_benchmark <- mean(benchmark_totals, na.rm = TRUE)
  
  # Return a list containing the mean values
  return(list(mean_portfolio = mean_portfolio, mean_benchmark = mean_benchmark))
}

# Vector of .rds file names
file_names <- c("MV 10 assets.rds", "MV 25 assets.rds", "MV 50 assets.rds", "CVaR 10 assets.rds", 
                "CVaR 25 assets.rds", "CVaR 50 assets.rds")

# Use lapply to process all files
results_backtests <- lapply(file_names, process_rds_file)

# If you want to combine the results into a data frame
results_backtests_df <- do.call(rbind, results_backtests)
rownames(results_backtests_df) <- file_names

# Print the results data frame
print(results_backtests_df)

