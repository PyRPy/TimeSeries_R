# CHAPTER 7
# Backtesting with Quantstrat
# https://github.com/hgeorgako/rfortraders
# codes from website above; outdated part modified or removed
# demoData.R part included in this code

#################
# Initial setup #
#################

library(quantmod)
library(PerformanceAnalytics)
library(blotter)
library(quantstrat)

# Suppresses warnings
options("getSymbols.warning4.0" = FALSE)
initDate = "2010-01-01"
from = "2010-01-01"
to = "2021-12-31"

# Do some house cleaning
rm(list = ls(.blotter), envir = .blotter)

# Set the currency and the timezone
currency('USD')
Sys.setenv(TZ = "UTC")

# Define symbols of interest
symbols <- c("XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "RWR", #SPDR Dow Jones REIT ETF
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IYR", #iShares U.S. Real Estate
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)

# SPDR ETFs first, iShares ETFs afterwards
if(!"XLB" %in% ls()) {
  # If data is not present, get it from yahoo
  suppressMessages(getSymbols(symbols, from = from,
                              to = to,  src = "yahoo", adjust = TRUE))
}

# Define the instrument type
stock(symbols, currency = "USD", multiplier = 1)

###############################################
# The first strategy: A simple trend follower #
###############################################
"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) {
  ATR <- ATR(HLC, n = n, maType = maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- "atr"
  return(out)
}

"osDollarATR" <- function(orderside, tradeSize, pctATR,
                          maxPctATR = pctATR,  data, timestamp,
                          symbol, prefer = "Open", portfolio, integerQty = TRUE,
                          atrMod = "", rebal = FALSE, ...) {
  if(tradeSize > 0 & orderside == "short"){
    tradeSize <- tradeSize * -1
  }
  
  pos <- getPosQty(portfolio, symbol, timestamp)
  atrString <- paste0("atr", atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  
  if(length(atrCol) == 0) {
    stop(paste("Term", atrString,
               "not found in mktdata column names."))
  }
  
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if(is.na(atrTimeStamp) | atrTimeStamp == 0) {
    stop(paste("ATR corresponding to", atrString,
               "is invalid at this point in time.  Add a logical
    operator to account for this."))
  }
  
  dollarATR <- pos * atrTimeStamp
  
  desiredDollarATR <- pctATR * tradeSize
  remainingRiskCapacity <- tradeSize *
    maxPctATR - dollarATR
  
  if(orderside == "long"){
    qty <- min(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  } else {
    qty <- max(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  }
  
  if(integerQty) {
    qty <- trunc(qty)
  }
  if(!rebal) {
    if(orderside == "long" & qty < 0) {
      qty <- 0
    }
    if(orderside == "short" & qty > 0) {
      qty <- 0 }
  }
  if(rebal) {
    if(pos == 0) {
      qty <- 0
    } 
  }
  return(qty)
}

options(width = 70)

# To rerun the strategy, rerun everything below this line
# demoData.R contains all of the data-related boilerplate.
# source("demoData.R")

# Trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize * length(symbols)

rm(list=c(.blotter, .strategy))
.blotter <<- new.env()
.strategy <<- new.env() 

strategy.st <- "Clenow_Simple"
portfolio.st <- "Clenow_Simple"
account.st <- "Clenow_Simple"


rm.strat(portfolio.st)
rm.strat(strategy.st)

if (!exists('.blotter')) .blotter <- new.env()
initPortf(portfolio.st, symbols = symbols,
          initDate = initDate, currency = 'USD')

initAcct(account.st, portfolios = portfolio.st,
         initDate = initDate, currency = 'USD', initEq = initEq)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store=TRUE)

##################################
# Backtesting the first strategy #
##################################
nLag = 252
pctATR = 0.02
period = 10

namedLag <- function(x, k = 1, na.pad = TRUE, ...) {
  out <- lag(x, k = k, na.pad = na.pad, ...)
  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "namedLag"
  return(out)
}

add.indicator(strategy.st, name = "namedLag",
              arguments = list(x = quote(Cl(mktdata)), k = nLag),
              label = "ind")

add.indicator(strategy.st, name = "lagATR",
              arguments = list(HLC = quote(HLC(mktdata)), n = period),
              label = "atrX")

test <- applyIndicators(strategy.st, mktdata = OHLC(XLB))
head(round(test, 2), 253)

# Signals
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("Close", "namedLag.ind"),
                            relationship = "gt"),
           label = "coverOrBuy")

add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("Close", "namedLag.ind"),
                            relationship = "lt"),
           label = "sellOrShort")

# Long rules
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "coverOrBuy",
                          sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE,
                          prefer = "Open", osFUN = osDollarATR,
                          tradeSize = tradeSize, pctATR = pctATR,
                          atrMod = "X"), type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sellOrShort",
                          sigval = TRUE, orderqty = "all",
                          ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open"),
         type = "exit", path.dep = TRUE)

# Short rules
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sellOrShort",
                          sigval = TRUE, ordertype = "market",
                          orderside = "short", replace = FALSE,
                          prefer = "Open", osFUN = osDollarATR,
                          tradeSize = -tradeSize, pctATR = pctATR,
                          atrMod = "X"), type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "coverOrBuy",
                          sigval = TRUE, orderqty = "all",
                          ordertype = "market", orderside = "short",
                          replace = FALSE, prefer = "Open"),
         type = "exit", path.dep = TRUE)

# Get begin time
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st,
                     portfolios = portfolio.st)

# Record end time
t2 <- Sys.time()
print(t2 - t1)

# [1] "2020-03-12 00:00:00 XLY -44 @ 97.3797682392326"
# [1] "2020-05-11 00:00:00 XLY 44 @ 114.811207402566"
# [1] "2020-05-11 00:00:00 XLY 66 @ 114.811207402566"
# [1] "2020-05-14 00:00:00 XLY -66 @ 109.724507833407"
# [1] "2020-05-14 00:00:00 XLY -69 @ 109.724507833407"
# [1] "2020-05-15 00:00:00 XLY 69 @ 111.828331388389"
# [1] "2020-05-15 00:00:00 XLY 67 @ 111.828331388389"

##############################
# Evaluating the performance #
##############################
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)

print(data.frame(t(tStats[,-c(1,2)])))
aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses)
aggCorrect <- mean(tStats$Percent.Positive)
numTrades <- sum(tStats$Num.Trades)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)

tStats

aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses)
## [1] 3.663545

aggCorrect <- mean(tStats$Percent.Positive)
## [1] 36.00233

numTrades <- sum(tStats$Num.Trades)
## [1] 1134

meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)
## [1] 9.871333

instRets <- PortfReturns(account.st)
portfRets <- xts(rowMeans(instRets) * ncol(instRets),
                 order.by = index(instRets))
portfRets <- portfRets[!is.na(portfRets)]
cumPortfRets <- cumprod(1 + portfRets)
firstNonZeroDay <- as.character(index(portfRets)[
  min(which(portfRets != 0))])

# Obtain symbol
getSymbols("SPY", from = firstNonZeroDay, to = to)
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1 + SPYrets)
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison)  <- c("strategy", "SPY")
# chart.TimeSeries(comparison, legend.loc = "topleft",
#                  colors=c("green", "red"))

plot.xts(comparison, legend.loc = "topleft",
         colors=c("green", "red"))

# Calculate risk metrics
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

SharpeRatio.annualized(portfRets)
## [1] 0.6424366

Return.annualized(portfRets)
## [1] 0.1392711

maxDrawdown(portfRets)
## [1] 0.278221

chart.Posn(portfolio.st, "XLB")
tmp <- namedLag(Cl(XLB), k = nLag)
add_TA(tmp$namedLag, col = "blue", on = 1)
