# template_quantstrat_tutorials_Dr_Ko.R -----------------------------------
# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/

# install.packages("devtools")
# install.packages("quantmod")
# install.packages("FinancialInstrument")
# install.packages("PerformanceAnalytics")
# install.packages("foreach")
# Install from github directly
# install_github("braverock/blotter")
# install_github("braverock/quantstrat")

library(blotter)
library(quantstrat)
library(FinancialInstrument)
library(quantmod)


# Scenario - SMA20 and SMA50 crossover effect for short-term tradi --------


# Step 1: Initialization --------------------------------------------------

options("getSymbols.warning4.0"=FALSE)
from ="2019-01-01"
to ="2022-03-11"
initDate = "2014-12-01"
symbols = c("SPY")
sampledStocks <- data.frame(Symbol = symbols)

# store data in a new environment
stocksEnv <- new.env()
getSymbols(sampledStocks$Symbol, env = stocksEnv, 
           from = "2021-01-01", src = "yahoo")


for (stock in sampledStocks$Symbol) {
  chartSeries(stocksEnv[[stock]], 
              theme="white", 
              name = stock,
              TA="addVo();
              addBBands();
              addRSI(14); 
              addSMA(20, col='red'); 
              addSMA(50, col='blue'); 
              addSMA(200, col='black')")
}

currency("USD")
getSymbols(symbols, from=from, to=to, adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6
if (!exists('.blotter')) .blotter <- new.env()

strategy.st <- portfolio.st <- account.st <- "SMAv"
rm.strat(strategy.st)


initPortf(portfolio.st, symbols=symbols,
          initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, 
         initDate=initDate, currency='USD',
         initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)


# Step 2: Indicator -------------------------------------------------------
# 52 period standard deviation of close prices is less than its median 
# over the last N periods.

RB <- function(x,n){
  x <- x
  sd <- runSD(x, n, sample= FALSE)
  med <- runMedian(sd,n)
  mavg <- SMA(x,n)
  signal <- ifelse(sd < med & x > mavg,1,0)
  colnames(signal) <- "RB"
  reclass(signal,x)
}


add.indicator(
  strategy.st, name="SMA",
  arguments=list(x=quote(Cl(mktdata)), n=20),
  label="sma20")

add.indicator(
  strategy.st, name="SMA",
  arguments=list(x=quote(Cl(mktdata)), n=200),
  label="sma200")

add.indicator(
  strategy.st, name="RB",
  arguments=list(x=quote(Cl(mktdata)), n=50),
  label="RB")


# Step 3: Signals ---------------------------------------------------------

# Bull market if RB>=1
add.signal(strategy.st, 
           name="sigThreshold", 
           arguments = list(threshold=1, column="RB",
                            relationship="gte",
                            cross=TRUE),
           label="buy")

# Sell market if SMA20 < SMA50
add.signal(strategy.st, 
           name="sigComparison",
           arguments=list(columns=c("sma20","sma50"), 
                          relationship="lt"), 
           label="sell")


# Step 4: Rules -----------------------------------------------------------

# Buy Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="buy", 
                          sigval=TRUE,  
                          orderqty=1000, 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE)

# Sell Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="sell", 
                          sigval=TRUE,  
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          replace=FALSE), 
         type='exit', 
         path.dep=TRUE) 


# Step 5: Evaluation ------------------------------------------------------
out<-try(applyStrategy(strategy.st, 
                       portfolios=portfolio.st))


updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(account.st)

for(symbol in symbols) {
  chart.Posn(Portfolio=portfolio.st,
             Symbol=symbol,log=TRUE)
}

ret <- PortfReturns(Account = "SMAv")
charts.PerformanceSummary(
  ret, geometric = FALSE,
  wealth.index = TRUE,
  main = "SMA and Volatility"
)
