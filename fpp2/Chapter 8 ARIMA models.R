# Chapter 8 ARIMA models
# 8.1 Stationarity and differencing
# Differencing
library(fpp2)
Box.test(diff(goog200), lag=10, type="Ljung-Box")
ggAcf(goog200)
ggAcf(diff(goog200))

# Random walk model
# Seasonal differencing
# example :  A10 (antidiabetic) drugs sold in Australia
cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")

cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" =
        diff(log(usmelec),12),
      "Doubly\n differenced logs" =
        diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")

# Unit root tests
library(urca)
goog %>% ur.kpss() %>% summary()
# the data are not stationary. We can difference 
# the data, and apply the test again.
goog %>% diff() %>% ur.kpss() %>% summary()

ndiffs(goog)

usmelec %>% log() %>% nsdiffs() # seasonal diff

usmelec %>% log() %>% diff(lag=12) %>% ndiffs()

# 8.2 Backshift notation
# 8.3 Autoregressive models
# 8.4 Moving average models
# 8.5 Non-seasonal ARIMA models
# example: US consumption expenditure
autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")

fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE)
fit %>% forecast(h=10) %>% autoplot(include=80)

# Understanding ARIMA models
# ACF and PACF plots
ggAcf(uschange[,"Consumption"])
ggPacf(uschange[,"Consumption"])

(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))
(fit3 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                    stepwise=FALSE, approximation=FALSE))
# 8.6 Estimation and order selection
# 8.7 ARIMA modelling in R
# Example: Seasonally adjusted electrical equipment orders
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)
eeadj %>% diff() %>% ggtsdisplay(main="")
(fit <- Arima(eeadj, order=c(3,1,1)))
checkresiduals(fit)
autoplot(forecast(fit))

# Plotting the characteristic roots
autoplot(fit)

# 8.8 Forecasting
# 8.9 Seasonal ARIMA models
# Example: European quarterly retail trade
autoplot(euretail) + ylab("Retail index") + xlab("Year")
euretail %>% diff(lag=4) %>% ggtsdisplay() # non-stationary
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()

euretail %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

# Both the ACF and PACF show significant spikes at lag 2, and 
# almost significant spikes at lag 3, indicating that some 
# additional non-seasonal terms need to be included in the model.
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)

fit3 %>% forecast(h=12) %>% autoplot()
# auto.arima
auto.arima(euretail)

# Example: Corticosteroid drug sales in Australia
lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales"=lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("")

lh02 %>% diff(lag=12) %>%
  ggtsdisplay(xlab="Year",
              main="Seasonally differenced H02 scripts")

# In the plots of the seasonally differenced data, there are 
# spikes in the PACF at lags 12 and 24, but nothing at 
# seasonal lags in the ACF. This may be suggestive of a 
# seasonal AR(2) term. In the non-seasonal lags, there are 
# three significant spikes in the PACF, suggesting a possible 
# AR(3) term. The pattern in the ACF is not indicative of 
# any simple model.
(fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2),
              lambda=0))
checkresiduals(fit, lag=36)

h02 %>%
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")

# 8.10 ARIMA vs ETS
# Example: Comparing auto.arima() and ets() on non-seasonal data
fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

air <- window(ausair, start=1990)
# Compute CV errors for ETS as e1
e1 <- tsCV(air, fets, h=1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(air, farima, h=1)

# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
air %>% ets() %>% forecast() %>% autoplot()

# Example: Comparing auto.arima() and ets() on seasonal data
# Consider the qcement data beginning in 1988
cement <- window(qcement, start=1988)

# Use 20 years of the data as the training set
train <- window(cement, end=c(2007,4))
# arima
(fit.arima <- auto.arima(train))
checkresiduals(fit.arima)

# ets
(fit.ets <- ets(train))
checkresiduals(fit.ets)

# Generate forecasts and compare accuracy over the test set
a1 <- fit.arima %>% forecast(h = 4*(2013-2007)+1) %>%
  accuracy(qcement)
a1[,c("RMSE","MAE","MAPE","MASE")]

a2 <- fit.ets %>% forecast(h = 4*(2013-2007)+1) %>%
  accuracy(qcement)
a2[,c("RMSE","MAE","MAPE","MASE")]

# Generate forecasts from an ETS model
cement %>% ets() %>% forecast(h=12) %>% autoplot()
