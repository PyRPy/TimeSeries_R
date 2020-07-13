# Sampling of stocks ------------------------------------------------------

library(readr)
library(quantmod)
companylist <- read_csv("symbols500.csv") # prepared in excel
head(companylist)
stockSymbols <- companylist["Symbol"]

numberStocks <- nrow(stockSymbols)
idx <- sample(1:numberStocks, 100)
idx
sampledStocks <- stockSymbols[idx, "Symbol"]
sampledStocks

stocksEnv <- new.env()
getSymbols(sampledStocks$Symbol, env = stocksEnv, from = "2020-01-01", src = "yahoo")

goodCandidate <- data.frame()


for (stock in sampledStocks$Symbol) {
  dret <- dailyReturn(stocksEnv[[stock]])
  pret <- Return.cumulative(dret)
  if (sd(dret) < 0.05 && as.numeric(pret) > 0.15) {
    tmp <- data.frame(Symbol = stock, sd = round(sd(dret), 3), ret = round(as.numeric(pret), 3))
    goodCandidate <- rbind(goodCandidate, tmp)
    
  }
}

goodCandidate
