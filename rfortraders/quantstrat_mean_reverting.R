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
# The second strategy: Cumulative Connors RSI #
###############################################
# Compute Connor's RSI, depends on RSI TTR function
connorsRSI <- function(price, nRSI = 3, nStreak = 2,
  nPercentLookBack = 100 ) {
  priceRSI <- RSI(price, nRSI)
  streakRSI <- RSI(computeStreak(price), nStreak)
  percents <- round(runPercentRank(x = diff(log(price)),
    n = 100, cumulative = FALSE, exact.multiplier = 1) * 100)
  ret <- (priceRSI + streakRSI + percents) / 3
  colnames(ret) <- "connorsRSI"
  return(ret)
}

# Computes a running streak of positives and
# negatives of price changes
computeStreak <- function(priceSeries) {
  signs <- sign(diff(priceSeries))
  posDiffs <- negDiffs <- rep(0,length(signs))
  posDiffs[signs == 1] <- 1
  negDiffs[signs == -1] <- -1

  # Create vector of cumulative sums and cumulative
  # sums not incremented during streaks.
  # Zero out any leading NAs after na.locf
  posCum <- cumsum(posDiffs)
  posNAcum <- posCum
  posNAcum[posDiffs == 1] <- NA
  posNAcum <- na.locf(posNAcum, na.rm = FALSE)
  posNAcum[is.na(posNAcum)] <- 0
  posStreak <- posCum - posNAcum

  # Repeat for negative cumulative sums
  negCum <- cumsum(negDiffs)
  negNAcum <- negCum
  negNAcum[negDiffs == -1] <- NA
  negNAcum <- na.locf(negNAcum, na.rm = FALSE)
  negNAcum[is.na(negNAcum)] <- 0
  negStreak <- negCum - negNAcum

  streak <- posStreak + negStreak
  streak <- xts(streak, order.by = index(priceSeries))
  return (streak)
}

sigAND <- function(label, data=mktdata,
  columns,  cross = FALSE) {
  ret_sig = NULL
  colNums <- rep(0, length(columns))
  for(i in 1:length(columns)) {
    colNums[i] <- match.names(columns[i], colnames(data))
  }
  ret_sig <- data[, colNums[1]]
  for(i in 2:length(colNums)) {
    ret_sig <- ret_sig & data[, colNums[i]]
  }
  ret_sig <- ret_sig * 1
  if (isTRUE(cross))
    ret_sig <- diff(ret_sig) == 1
  colnames(ret_sig) <- label
  return(ret_sig)
}

cumCRSI <- function(price, nCum = 2, ...) {
  CRSI <- connorsRSI(price, ...)
  out <- runSum(CRSI, nCum)
  colnames(out) <- "cumCRSI"
  out
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

"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) {
  ATR <- ATR(HLC, n = n, maType = maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- "atr"
  return(out)
}

initEq = 10000
tradeSize <- 10000

currency('USD')
Sys.setenv(TZ="UTC")

strategy.st <- "CRSIcumStrat"
portfolio.st <- "CRSIcumStrat"
account.st <- "CRSIcumStrat"

rm.strat(portfolio.st)
rm.strat(strategy.st)

if (!exists('.blotter')) .blotter <- new.env()
initPortf(portfolio.st, symbols = symbols,
  initDate = initDate, currency = 'USD')

initAcct(account.st, portfolios = portfolio.st,
  initDate = initDate, currency = 'USD',
  initEq = initEq)

initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

# Parameters
cumThresh <- 40
exitThresh <- 75
nCum <- 2
nRSI <- 3
nStreak <- 2
nPercentLookBack <- 100
nSMA <- 200
pctATR <- .02
period <- 10

# Indicators
add.indicator(strategy.st, name = "cumCRSI",
  arguments = list(price = quote(Cl(mktdata)), nCum = nCum,
  nRSI = nRSI, nStreak = nStreak,
  nPercentLookBack = nPercentLookBack),
  label = "CRSIcum")

add.indicator(strategy.st, name = "connorsRSI",
  arguments = list(price = quote(Cl(mktdata)), nRSI = nRSI,
  nStreak = nStreak,
  nPercentLookBack = nPercentLookBack),
  label = "CRSI")

add.indicator(strategy.st, name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n = nSMA),
  label = "sma")

add.indicator(strategy.st, name = "lagATR",
    arguments = list(HLC = quote(HLC(mktdata)), n = period),
    label = "atrX")

# Signals
add.signal(strategy.st, name = "sigThreshold",
  arguments = list(column = "cumCRSI.CRSIcum",
  threshold = cumThresh, relationship = "lt", cross = FALSE),
  label="cumCRSI.lt.thresh")

add.signal(strategy.st, name = "sigComparison",
  arguments = list(columns = c("Close", "SMA.sma"),
  relationship = "gt"), label = "Cl.gt.SMA")

add.signal(strategy.st, name = "sigAND",
  arguments = list(columns = c("cumCRSI.lt.thresh",
  "Cl.gt.SMA"), cross = TRUE), label = "longEntry")

add.signal(strategy.st, name = "sigThreshold",
  arguments = list(column = "connorsRSI.CRSI",
  threshold = exitThresh, relationship = "gt",
  cross = TRUE), label = "longExit")

# Rules
add.rule(strategy.st, name = "ruleSignal",
  arguments = list(sigcol = "longEntry", sigval = TRUE,
  ordertype = "market", orderside  ="long", replace = FALSE,
  prefer = "Open", osFUN = osDollarATR, tradeSize = tradeSize,
  pctATR = pctATR, atrMod = "X"), type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal",
  arguments = list(sigcol = "longExit", sigval = TRUE,
  orderqty = "all", ordertype = "market", orderside = "long",
  replace = FALSE, prefer = "Open"), type = "exit", path.dep = TRUE)

# Apply Strategy
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st,
  portfolios = portfolio.st)
t2 <- Sys.time()
print(t2 - t1)

# Set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

##########################################
# Evaluating the mean-reverting strategy #
##########################################

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)

print(data.frame(t(tStats[,-c(1,2)])))

aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses)
## [1] 1.501

aggCorrect <- mean(tStats$Percent.Positive)
## [1] 70.72

numTrades <- sum(tStats$Num.Trades)
## [1] 1566

meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)
## [1] 0.68

dStats <- dailyStats(Portfolios = portfolio.st, use = "Equity")
rownames(dStats) <- gsub(".DailyEndEq", "", rownames(dStats))
print(data.frame(t(dStats)))

durationStatistics <- function(Portfolio, Symbols,
  includeOpenTrade = FALSE, ...) {

  tmp <- list()
  length(tmp) <- length(Symbols)
  for(Symbol in Symbols) {
    pts <- perTradeStats(Portfolio = Portfolio,
      Symbol = Symbol, includeOpenTrade = includeOpenTrade)
    pts$diff <- pts$End - pts$Start

    durationSummary <- summary(as.numeric(pts$diff))
    winDurationSummary <- summary(as.numeric(
      pts$diff[pts$Net.Trading.PL > 0]))
    lossDurationSummary <- summary(as.numeric(
      pts$diff[pts$Net.Trading.PL <= 0]))
    names(durationSummary) <-
      c("Min", "Q1", "Med", "Mean", "Q3", "Max")
    names(winDurationSummary) <-
      c("Min", "Q1", "Med", "Mean", "Q3", "Max")
    names(lossDurationSummary) <-
      c("Min", "Q1", "Med", "Mean", "Q3", "Max")
    names(winDurationSummary) <-
      paste0("W", names(winDurationSummary))
    names(lossDurationSummary) <-
      paste0("L", names(lossDurationSummary))
    dataRow <- data.frame(cbind(t(round(durationSummary)),
      t(round(winDurationSummary)),
      t(round(lossDurationSummary))))
    tmp[[Symbol]] <- dataRow
  }
  out <- do.call(rbind, tmp)
  return(out)
}

durStats <- durationStatistics(Portfolio=portfolio.st,
  Symbols=sort(symbols))

print(t(durStats))

# Market exposure - need to fix
# tmp <- list()
# length(tmp) <- length(symbols)
# for(i in 1:nrow(dStats)) {
#   totalDays <- nrow(get(rownames(dStats)[i]))
#   mktExposure <- dStats$Total.Days[i] / totalDays
#   tmp[[i]] <- c(rownames(dStats)[i], round(mktExposure, 3))
# }
# mktExposure <- data.frame(do.call(rbind, tmp))
# colnames(mktExposure) <- c("Symbol", "MktExposure")
# 
# print(mktExposure)
# 
# print(mean(as.numeric(as.character(mktExposure$MktExposure))))
## [1] 0.1257

instRets <- PortfReturns(account.st)
portfRets <- xts(rowMeans(instRets) * ncol(instRets),
                 order.by = index(instRets))
portfRets <- portfRets[!is.na(portfRets)]
cumPortfRets <- cumprod(1 + portfRets)
firstNonZeroDay <- as.character(index(portfRets)[
  min(which(portfRets != 0))])

SharpeRatio.annualized(window(portfRets, start = "2010-10-20"))
## [1]  0.6973019

Return.annualized(window(portfRets, start = "2010-10-20"))
## [1] 0.03370045

maxDrawdown(portfRets)
## [1] 0.09120687

chart.Posn(portfolio.st, "XLB")
TA_CRSI <- connorsRSI(Cl(XLB), nRSI = nRSI,
  nStreak = nStreak, nPercentLookBack = nPercentLookBack)
add_TA(TA_CRSI, col = "red")

TA_cumCRSI <- cumCRSI(price = Cl(XLB),
  nCum = nCum, nRSI = nRSI, nStreak = nStreak,
  nPercentLookBack = nPercentLookBack)
add_TA(TA_cumCRSI, col = "blue")

TA_lagATR <- lagATR(HLC = HLC(XLB), n = period)
add_TA(TA_lagATR, col = "purple")

TA_SMA <- SMA(Cl(XLB), n = nSMA)
add_TA(TA_SMA, col = "blue", lwd = 2, on = 1)
