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


# setup the account -------------------------------------------------------

from ="2020-01-01"
to ="2020-06-12"
symbols = c("MSFT")
currency("USD")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^4

rm("account.filter",pos=.blotter)
rm("portfolio.filter",pos=.blotter)

initPortf("filter", symbol=symbols)
initAcct("filter", portfolios = "filter",
         initEq = initEq)


# generate trading indicator ----------------------------------------------

price <- Cl(MSFT)         
r <- price/Lag(price) - 1    
delta<-0.03
signal <-c(NA)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))


#  convert trading indicator to trading signal ----------------------------
trade <- Lag(signal)
trade <- na.fill(trade,0)


# apply trading signal into trading action --------------------------------

for (i in 1:length(price)){
  if (as.numeric(trade[i]) == 1){
    addTxn(Portfolio = "filter",
           Symbol = "MSFT", 
           TxnDate = time(price[i]), 
           TxnQty = 50,
           TxnPrice = price[i],
           TxnFees = 0)    
  }
  if (as.numeric(trade[i]) == -1){
    addTxn(Portfolio = "filter",
           Symbol = "MSFT", 
           TxnDate = time(price[i]), 
           TxnQty = -50,
           TxnPrice = price[i],
           TxnFees = 0)    
  }
}


# update the account and do charting --------------------------------------

updatePortf(Portfolio = "filter")
updateAcct(name = "filter")
updateEndEq(Account = "filter")
chart.Posn("filter", Symbol = "MSFT")
