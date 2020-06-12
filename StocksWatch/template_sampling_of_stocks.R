# Sampling of stocks ------------------------------------------------------

library(readr)
library(quantmod)
companylist <- read_csv("companylist.csv")
stockSymbols <- companylist["Symbol"]

numberStocks <- nrow(stockSymbols)
idx <- sample(1:numberStocks, 10)
idx
sampledStocks <- stockSymbols[idx, "Symbol"]
sampledStocks

# store data in a new environment
stocksEnv <- new.env()
getSymbols(sampledStocks$Symbol, env = stocksEnv, from = "2019-01-01", src = "yahoo")


# plot for trend checking
lineChart(stocksEnv$NOMD)
addSMA(n = 10, col = "blue")
addSMA(n = 50, col = "red")
addSMA(n = 200, col = "yellow")
zoomChart('2020-06::')


stocksTrend <- function(stock) {
  lineChart(stock)
  addSMA(n = 10, col = "blue")
  addSMA(n = 50, col = "red")
}

# for (stock in sampledStocks){
#   lineChart(stocksEnv$stock)
# }
