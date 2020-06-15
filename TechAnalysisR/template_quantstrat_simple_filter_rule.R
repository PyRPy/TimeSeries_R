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



# Step 1: Initialization --------------------------------------------------

options("getSymbols.warning4.0"=FALSE)
from ="2020-01-01"
to ="2020-06-12"
symbols = c("MSFT", "IBM")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)

currency("USD")
stock(symbols, currency="USD", multiplier=1)

# define our strategy, portfolio and account name

strategy.st <- "filter"
portfolio.st <- "filter"
account.st <- "filter"

# Remove any old variables

rm.strat("filter")

initEq=100000
initDate="2010-01-01"

initPortf(name=portfolio.st, 
          symbols=symbols, 
          initDate=initDate, 
          currency='USD')
initAcct(name=account.st, 
         portfolios=portfolio.st,    
         initDate=initDate, 
         currency='USD', 
         initEq=initEq)
initOrders(portfolio=portfolio.st, 
           symbols=symbols,
           initDate=initDate)

strategy(strategy.st, store=TRUE)


#  Step 2: Define indicator -----------------------------------------------

filter <- function(price) {
  lagprice <- lag(price,1)
  temp<-price/lagprice - 1
  colnames(temp) <- "filter"
  return(temp)
} 

add.indicator(
  strategy=strategy.st,
  name = "filter", 
  arguments = list(price = quote(Cl(mktdata))), 
  label= "filter")

# check whether it is correctly defined
test <-try(applyIndicators(strategy.st, 
                           mktdata=OHLC(MSFT)))
head(test, n=4)


# Step 3: Trading Signals -------------------------------------------------

# enter when filter > 1+\delta
add.signal(strategy.st, 
           name="sigThreshold",
           arguments = list(threshold=0.05,   
                            column="filter",
                            relationship="gt",   
                            cross=TRUE),
           label="filter.buy")

# exit when filter < 1-delta
add.signal(strategy.st, 
           name="sigThreshold",
           arguments = list(threshold=-0.05, 
                            column="filter",
                            relationship="lt",
                            cross=TRUE),
           label="filter.sell") 


# Step 4. Trading Rules ---------------------------------------------------

add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="filter.buy", 
                          sigval=TRUE,  
                          orderqty=1000,
                          ordertype='market', 
                          orderside='long',
                          pricemethod='market',
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE)

add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="filter.sell",
                          sigval=TRUE, 
                          orderqty=-1000,  
                          ordertype='market',  
                          orderside='long', 
                          pricemethod='market',  
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE) 


# Step 5. Evaluation Results ----------------------------------------------

out<-try(applyStrategy(strategy=strategy.st,
                       portfolios=portfolio.st))

# update the portfolio, account and equity

updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(account.st)

# trading position, profit and loss, and drawdown

for(symbol in symbols) {
  chart.Posn(Portfolio=portfolio.st,
             Symbol=symbol,
             log=TRUE)
}

# details of trades stats

tstats <- tradeStats(portfolio.st)
t(tstats) #transpose tstats

# evaluate the performance
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
tab <- table.Arbitrary(rets,
                       metrics=c(
                         "Return.cumulative",
                         "Return.annualized",
                         "SharpeRatio.annualized",
                         "CalmarRatio"),
                       metricsNames=c(
                         "Cumulative Return",
                         "Annualized Return",
                         "Annualized Sharpe Ratio",
                         "Calmar Ratio"))
tab

charts.PerformanceSummary(rets, colorset = bluefocus)
