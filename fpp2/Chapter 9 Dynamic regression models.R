# Chapter 9 Dynamic regression models
# 9.1 Estimation
library(forecast)
#fit <- Arima(y, xreg = x, order = c(1, 1, 0))

# 9.2 regression with Arima errors in R
# example : US personal consumption and income
autoplot(uschange[, 1:2], facets = TRUE) +
  xlab("year")+
  ylab("")+
  ggtitle("Quarterly changes in US consumption and personal income")

fit <- auto.arima(uschange[, "Consumption"], xreg = uschange[, "Income"])
fit

# find residules
cbind("Regression Errors" = residuals(fit, type = "regression"),
      "ARIMA errors" = residuals(fit, type = "innovation")) %>% 
  autoplot(facets=TRUE)

## It is the ARIMA errors that should resemble a white noise series.

checkresiduals(fit)

# 9.3 Forecasting
fcast <- forecast(fit, xreg = rep(mean(uschange[, 2]), 8))
autoplot(fcast) +
  xlab("Year") +
  ylab("Percentage change")

## exmaple : forecasting electricity demand
xreg <- cbind(MaxTemp=elecdaily[,"Temperature"],
              MaxTempSq=elecdaily[, "Temperature"]^2,
              Workday=elecdaily[, "WorkDay"])

fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)
## The model has some significant autocorrelation in the residuals, which means 
## the prediction intervals may not provide accurate coverage.

fcast <- forecast(fit,
                  xreg = cbind(MaxTemp=rep(26, 14), 
                               MaxTempSq=rep(26^2, 14),
                              Workday=c(0,1,0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1)))

autoplot(fcast) +
  ylab("Electricity demand (GW)")

# 9.4 stochastic and determinsitic trends
autoplot(austa) +
  xlab("Year") +
  ylab("millions of people") +
  ggtitle("total annal international visitors to Austrilia")

trend <- seq_along(austa)
fit <- auto.arima(austa, d=0, xreg = trend)
fit

fit2 <- auto.arima(austa, d=1)
fit2

# forecasting
fc1 <- forecast(fit, xreg=length(austa) + 1:10)
fc2 <- forecast(fit2, h=10)

autoplot(austa) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series = "Deterministic trend") +
  ggtitle("Forecast from trend models") +
  xlab("year") +
  ylab("Visitors to Austrilia (millions)") +
  guides(colour=guide_legend(title = "Forecast"))
