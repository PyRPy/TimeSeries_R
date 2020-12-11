# Building and Testing Stock Portfolios in R ------------------------------
# https://towardsdatascience.com/building-and-testing-stock-portfolios-in-r-d1b7b6f59ac4
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)

## Writing a function to calculate stock returns
monthly_returns <- function(ticker, base_year)
{
  # Obtain stock price data from Yahoo! Finance
  stock <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE) 
  # Remove missing values
  stock <- na.omit(stock)
  # Keep only adjusted closing stock prices
  stock <- stock[, 6]
  
  # Confine our observations to begin at the base year and end at the last available trading day
  horizon <- paste0(as.character(base_year), "/", as.character(Sys.Date()))
  stock <- stock[horizon]
  
  # Calculate monthly arithmetic returns
  data <- periodReturn(stock, period = "monthly", type = "arithmetic")
  
  # Assign to the global environment to be accessible
  assign(ticker, data, envir = .GlobalEnv)
}

## Using our function and visualizing returns
# Call our function for each stock
monthly_returns("SBUX", 2015)
monthly_returns("CCL", 2015)
monthly_returns("AAPL", 2015)

# Get S&P 500 Data
monthly_returns("SPY", 2015)

# Merge all the data and rename columns
returns <- merge.xts(SBUX, CCL, AAPL, SPY)
colnames(returns) <- c("SBUX", "CCL", "AAPL", "SP500")

# Produce interactive chart of stock returns
dygraph(returns, main = "Starbucks vs. Carnival vs. Apple vs. S&P 500") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,0.5)) %>%
  dyRangeSelector(dateWindow = c("2015-01-01", "2020-07-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 

# Print last 5 rows of the data, rounded to 4 decimal places
round(tail(returns, n = 5), 4)


# Analyzing portfolio composition -----------------------------------------

corrplot::corrplot(cor(returns), method = 'number') 


# Building our portfolio and assessing performance ------------------------

# Assign weights
wts <- c(1/3, 1/3, 1/3)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returns <- Return.portfolio(R = returns[,1:3], weights = wts, wealth.index = TRUE)

# Then isolate our S&P 500 data
benchmark_returns <- Return.portfolio(R = returns[,4], wealth.index = TRUE)

# Merge the two
comp <- merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")
