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
from ="2020-04-01"
to ="2020-06-12"
initDate = "2020-01-01"
initEq = 10000
symbols = c("SPY")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)

currency("USD")
stock(symbols, currency="USD", multiplier=1)

rm.strat("buyHold")

#Initial Setup
initPortf("buyHold", "SPY", initDate = initDate)
initAcct("buyHold", portfolios = "buyHold",
         initDate = initDate, initEq = initEq)


# Steps 2-4: Applying trading rule ----------------------------------------

# add the transaction to buy at the beginning:
FirstDate <- first(time(SPY))
# Enter order on the first date

BuyDate <- FirstDate
equity = getEndEq("buyHold", FirstDate)
FirstPrice <- as.numeric(Cl(SPY[BuyDate,]))
UnitSize = as.numeric(trunc(equity/FirstPrice))
addTxn("buyHold", Symbol = "SPY", 
       TxnDate = BuyDate, TxnPrice = FirstPrice,
       TxnQty = UnitSize, TxnFees = 0)


# add the transaction to sell at the end:

LastDate <- last(time(SPY))

# Exit order on the Last Date
LastPrice <- as.numeric(Cl(SPY[LastDate,]))
addTxn("buyHold", Symbol = "SPY", 
       TxnDate = LastDate, TxnPrice = LastPrice,
       TxnQty = -UnitSize , TxnFees = 0)


# Step 5: Evaluation ------------------------------------------------------

updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")

chart.Posn("buyHold", Symbol = "SPY")
