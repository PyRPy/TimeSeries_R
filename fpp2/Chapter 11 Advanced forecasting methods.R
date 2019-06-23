# Chapter 11 Advanced forecasting methods
# 11.1 Complex seasonality
library(fpp2)
p1 <- autoplot(calls) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(breaks=seq(1,33,by=2))

p2 <- autoplot(window(calls, end=4)) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(minor_breaks = seq(1,4,by=0.2))

gridExtra::grid.arrange(p1,p2)

# STL with multiple seasonal periods
calls %>% mstl() %>%
  autoplot() + xlab("Week")

calls %>%  stlf() %>%
  autoplot() + xlab("Week")

# Dynamic harmonic regression with multiple seasonal periods
fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
                  xreg=fourier(calls, K=c(10,10)))

fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")

# TBATS models
calls %>%
  subset(start=length(calls)-2000) %>%
  tbats() -> fit2

fc2 <- forecast(fit2, h=2*169)
autoplot(fc2, include=5*169) +
  ylab("Call volume") + xlab("Weeks")

# Complex seasonality with covariates
autoplot(elecdemand[,c("Demand","Temperature")],
         facet=TRUE) +
  scale_x_continuous(minor_breaks=NULL,
                     breaks=2014+
                       cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))/365,
                     labels=month.abb) +
  xlab("Time") + ylab("")

elecdemand %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) + geom_point() +
  xlab("Temperature (degrees Celsius)") +
  ylab("Demand (GW)")

cooling <- pmax(elecdemand[,"Temperature"], 18)
fit <- auto.arima(elecdemand[,"Demand"],
                  xreg = cbind(fourier(elecdemand, c(10,10,0)),
                               heating=elecdemand[,"Temperature"],
                               cooling=cooling))

temps <- subset(elecdemand[,"Temperature"],
                start=NROW(elecdemand)-2*48+1)
fc <- forecast(fit,
               xreg=cbind(fourier(temps, c(10,10,0)),
                          heating=temps, cooling=pmax(temps,18)))
autoplot(fc, include=14*48)

# 11.2 Vector autoregressions
# Example: A VAR model for forecasting US consumption
library(vars)
VARselect(uschange[,1:2], lag.max=8,
          type="const")[["selection"]]

var1 <- VAR(uschange[,1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- VAR(uschange[,1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

# Both a VAR(1) and a VAR(2) have some residual serial correlation, 
# and therefore we fit a VAR(3).
var3 <- VAR(uschange[,1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

forecast(var3) %>%
  autoplot() + xlab("Year")

# 11.3 Neural network models
# Neural network autoregression
# Example: sunspots
fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30))

# Prediction intervals
sim <- ts(matrix(0, nrow=30L, ncol=9L),
          start=end(sunspotarea)[1L]+1L)
for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=30L)
autoplot(sunspotarea) + autolayer(sim)

fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)

# 11.4 Bootstrapping and bagging
bootseries <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>% ts(start=2000, frequency=12)

autoplot(debitcards) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(debitcards, colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")

# Prediction intervals from bootstrapped series
nsim <- 1000L
sim <- bld.mbb.bootstrap(debitcards, nsim)

h <- 36L
future <- matrix(0, nrow=nsim, ncol=h)
for(i in seq(nsim))
  future[i,] <- simulate(ets(sim[[i]]), nsim=h)

start <- tsp(debitcards)[2]+1/12
simfc <- structure(list(
  mean = ts(colMeans(future), start=start, frequency=12),
  lower = ts(apply(future, 2, quantile, prob=0.025),
             start=start, frequency=12),
  upper = ts(apply(future, 2, quantile, prob=0.975),
             start=start, frequency=12),
  level=95),
  class="forecast")

etsfc <- forecast(ets(debitcards), h=h, level=95)
autoplot(debitcards) +
  ggtitle("Monthly retail debit card usage in Iceland") +
  xlab("Year") + ylab("million ISK") +
  autolayer(simfc, series="Simulated") +
  autolayer(etsfc, series="ETS")

# Bagged ETS forecasts
sim <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>%
  ts(frequency=12, start=2000)
fc <- purrr::map(as.list(sim),
                 function(x){forecast(ets(x))[["mean"]]}) %>%
  as.data.frame() %>%
  ts(frequency=12, start=start)
autoplot(debitcards) +
  autolayer(sim, colour=TRUE) +
  autolayer(fc, colour=TRUE) +
  autolayer(debitcards, colour=FALSE) +
  ylab("Bootstrapped series") +
  guides(colour="none")

etsfc <- debitcards %>% ets() %>% forecast(h=36)
baggedfc <- debitcards %>% baggedETS() %>% forecast(h=36)
autoplot(debitcards) +
  autolayer(baggedfc, series="BaggedETS", PI=FALSE) +
  autolayer(etsfc, series="ETS", PI=FALSE) +
  guides(colour=guide_legend(title="Forecasts"))
