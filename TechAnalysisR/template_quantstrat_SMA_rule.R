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
from ="2012-01-01"
to ="2012-12-31"
symbols = c("IBM","MSFT")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)

currency("USD")
initEq = 10000 

strategy.st <- portfolio.st <- account.st <- "SMA"
rm.strat(strategy.st)

initPortf(portfolio.st, symbols)
initAcct(account.st, portfolios=portfolio.st, 
         initEq = initEq)
initOrders(portfolio.st)
strategy(strategy.st, store=TRUE)


# Step 2: Indicator -------------------------------------------------------

add.indicator(
  strategy.st, name="SMA",
  arguments=list(x=quote(Cl(mktdata)), n=30),
  label="sma30")

add.indicator(
  strategy.st, name="SMA",
  arguments=list(x=quote(Cl(mktdata)), n=200),
  label="sma200")


# Step 3: Signals ---------------------------------------------------------

# Bull market if SMA30>SMA200
add.signal(
  strategy.st, 
  name="sigComparison",
  arguments=list(columns=c("sma30","sma200"),
                 relationship="gt"),
  label="buy")

# Sell market if SMA30<SMA200
add.signal(
  strategy.st, 
  name="sigComparison",
  arguments=list(columns=c("sma30","sma200"), 
                 relationship="lt"), 
  label="sell")


# Step 4: Rules -----------------------------------------------------------

# Buy Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="buy", 
                          sigval=TRUE,  
                          orderqty=20, 
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
