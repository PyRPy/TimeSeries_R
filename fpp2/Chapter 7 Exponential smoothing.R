# Chapter 7 Exponential smoothing
# 7.1 Simple exponential smoothing
library(fpp2)
oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

# Example: Oil production
oildata <- window(oil, start=1996)
# Estimate parameters
fc <- ses(oildata, h=5)
# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

# 7.2 Trend methods
# Example: Air Passengers
air <- window(ausair, start=1990)

fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

# Example: Sheep in Asia
autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)

# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)

# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

fc <- holt(livestock, damped=TRUE)
# Estimated parameters:
fc[["model"]]

autoplot(fc) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

# 7.3 Holt-Winters' seasonal method
# Holt-Winters' additive method
# Holt-Winters' multiplicative method

# Example: International tourist visitor nights in Australia
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

# Holt-Winters' damped method
# hw(y, damped=TRUE, seasonal="multiplicative")

fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
         damped = TRUE, seasonal="multiplicative", h=35)

autoplot(hyndsight) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))

# 7.4 A taxonomy of exponential smoothing methods
# 7.5 Innovations state space models for exponential smoothing

# ETS(A,N,N): simple exponential smoothing with additive errors
# ETS(M,N,N): simple exponential smoothing with multiplicative errors
# ETS(A,A,N): Holt's linear method with additive errors
# ETS(M,A,N): Holt's linear method with multiplicative errors

# 7.6 Estimation and model selection
# Model selection
# Example: International tourist visitor nights in Australia
aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)
autoplot(fit)

cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

# 7.7 Forecasting with ETS models
fit %>% forecast(h=8) %>%
  autoplot() +
  ylab("International visitor night in Australia (millions)")

# Prediction intervals
# forecast(object, h=ifelse(object$m>1, 2*object$m, 10),
#          level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE,
#          npaths=5000, PI=TRUE, lambda=object$lambda, biasadj=NULL, ...)

