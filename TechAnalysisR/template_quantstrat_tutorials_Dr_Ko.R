# template_quantstrat_tutorials_Dr_Ko.R -----------------------------------

# install.packages("quantmod")
# install.packages("FinancialInstrument")
# install.packages("PerformanceAnalytics")
# install.packages("foreach")

# install.packages("devtools")
library(devtools)
# Install from github directly
# install_github("braverock/blotter")
# install_github("braverock/quantstrat")

library(blotter)
library(quantstrat)
library(FinancialInstrument)
library(quantmod)


# setup the account -------------------------------------------------------


options("getSymbols.warning4.0"=FALSE)
from ="2019-01-01"
to ="2020-06-13"
symbols = c("AAPL", "IBM")
currency("USD")
getSymbols(symbols, from=from, to = to)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6

rm("account.buyHold",pos=.blotter)
rm("portfolio.buyHold",pos=.blotter)

if (!exists('.blotter')) .blotter <- new.env()
initPortf("buyHold", symbol=symbols)
initAcct("buyHold", portfolios = "buyHold",
         initEq = initEq)


# Buy and hold ------------------------------------------------------------

Apple.Buy.Date <- first(time(AAPL))
Apple.Buy.Price <- as.numeric(Cl(AAPL[Apple.Buy.Date,]))
Apple.Sell.Date <- last(time(AAPL))
Apple.Sell.Price <- as.numeric(Cl(AAPL[Apple.Sell.Date,]))
Apple.Qty <- trunc(initEq/(2*Apple.Buy.Price))

IBM.Buy.Date <- first(time(IBM))
IBM.Buy.Price <- as.numeric(Cl(IBM[IBM.Buy.Date,]))
IBM.Sell.Date <- last(time(IBM))
IBM.Sell.Price <- as.numeric(Cl(IBM[IBM.Sell.Date,]))
IBM.Qty <- trunc(initEq/(2*IBM.Buy.Price))

# add buy transactions

addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Buy.Date, 
       TxnQty = Apple.Qty,
       TxnPrice = Apple.Buy.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Buy.Date, 
       TxnQty = IBM.Qty,
       TxnPrice = IBM.Buy.Price,
       TxnFees = 0)

# add the sell transactions:
addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Sell.Date, 
       TxnQty = -Apple.Qty,
       TxnPrice = Apple.Sell.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Sell.Date, 
       TxnQty = -IBM.Qty,
       TxnPrice = IBM.Sell.Price,
       TxnFees = 0)

# update the account based on the added transactions

updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")

# chart our trading positions
chart.Posn("buyHold", Symbol = "AAPL")
chart.Posn("buyHold", Symbol = "IBM")

# see the trading statistics
out <- perTradeStats("buyHold", "IBM")
t(out)

out <- perTradeStats("buyHold", "AAPL")
t(out)
