# Chapter 9 Dynamic regression models
# 9.1 Estimation
library(forecast)
library(fpp2)
library(dplyr)
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

# 9.5 Dynamic harmonic regression
cafe04 <- window(auscafe, start=2004)
plots <- list()
for (i in seq(6)){
  fit <- auto.arima(cafe04, xreg=fourier(cafe04, K=i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                    xreg=fourier(cafe04, K=i, h=24))) +
                    ylab("") +
                    ylim(1.5, 4.7)
  
}

gridExtra::grid.arrange(
  plots[[1]], plots[[2]], plots[[3]],
  plots[[4]], plots[[5]], plots[[6]], nrow=3)

# 9.6 Lagged predictors
## example : TV advertising and insurance quotations
head(insurance)
autoplot(insurance, facets = TRUE) +
  xlab("year") +
  ylab("") +
  ggtitle("Insurance advertising and quotations")

# lagged predictors, test 0, 1, 2, 3 lags
Advert <- cbind(
  AdLag0 = insurance[, "TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"], -1),
  AdLag2 = stats::lag(insurance[,"TV.advert"], -2),
  AdLag3 = stats::lag(insurance[,"TV.advert"], -3)) %>% 
  head(NROW(insurance))

# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1],
                   stationary = TRUE)

fit2 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1:2],
                   stationary = TRUE)

fit3 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1:3],
                   stationary = TRUE)

fit4 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1:4],
                   stationary = TRUE)

c(fit1[["aicc"]], fit2[["aicc"]], fit3[["aicc"]], fit4[["aicc"]])

## best model is fit2 with the lowest aicc
fit <- auto.arima(insurance[, 1], xreg = Advert[, 1:2],
                   stationary = TRUE)
fit

fc8 <- forecast(fit, h = 20,
        xreg = cbind(AdLag0 = rep(8, 20),
        AdLag1 = c(Advert[40,1], rep(8, 19))))

autoplot(fc8) +
  ylab("Quotes") +
  ggtitle("Forecast quotes with future advertising set to 8")


