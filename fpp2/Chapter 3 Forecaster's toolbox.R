# Chapter 3 Forecaster's toolbox
# https://otexts.com/fpp2/simple-methods.html
library(fpp2)
# Some simple forecasting methods
## set training data from 1992 to 2007 for beer data set
beer2 <- window(ausbeer, start=1992, end=c(2007, 4))

# plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
      series="Mean", PI=FALSE) +
      autolayer(naive(beer2, h=11),
      series="Naive", PI=FALSE) +
      autolayer(snaive(beer2, h=11),
      series="Seasonal naive", PI=FALSE) +
  xlab("Year") +
  ylab("Megalitres") +
  guides(colour=guide_legend(title = "Forecast"))

# example : google stock price
autoplot(goog200) +
  autolayer(meanf(goog200, h=40), series = "Mean", PI=FALSE) +
  autolayer(rwf(goog200, h = 40), series = "Naive", PI=FALSE) +
  autolayer(rwf(goog200, drift = TRUE, h=40), series = "Drift", PI=FALSE) +
  ggtitle("Google stock-daily endign 6 Dec 2013") +
  xlab("Day") +
  ylab("Closing price (US$)") +
  guides(colour=guide_legend(title = "Forecast"))

# Transformations
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))

autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

(lambda <- BoxCox.lambda(elec))

autoplot(BoxCox(elec, lambda))

# bias adjustment
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

# Residual diagnostics
autoplot(goog200) +
  xlab("Day") +
  ylab("Closing price USD") +
  ggtitle("Google stock - daily ending 6 Dec 2013")

res <- residuals(naive(goog200))
autoplot(res) + 
  xlab("Day") +
  ylab("") +
  ggtitle("Residuals from naive method")

gghistogram(res) + ggtitle("Histrogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

# lag=h, fitdf=K
Box.test(res, lag=10, fitdf = 0)
Box.test(res, lag=10, fitdf = 0, type="Lj")
checkresiduals(naive(goog200))

# 3.4 Evaluating forecast accuracy
window(ausbeer, start=1995)
subset(ausbeer, start=length(ausbeer) - 4*5)
subset(ausbeer, quarter = 1)
tail(ausbeer, 4*5)

# Forecast errors
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

# Time series validation
e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm = TRUE))
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm = TRUE))

# pipe operator
goog200 %>% tsCV(forecastfunction = rwf, drift=TRUE, h=1) -> e
e^2 %>% mean(na.rm=TRUE) %>% sqrt()
goog200 %>% rwf(drift=TRUE) %>% residuals() -> res
res^2 %>% mean(na.rm=TRUE) %>% sqrt()

# Example - using tsCV()
e <- tsCV(goog200, forecastfunction = naive, h=8)

mse <- colMeans(e^2, na.rm = T)
data.frame(h=1:8, MSE=mse) %>% 
  ggplot(aes(x=h, y=MSE)) + 
  geom_point()

# 3.5 prediction intervals
naive(goog200)
autoplot(naive(goog200))
# bootstrapped
naive(goog200, bootstrap = TRUE)

# 3.6 forecast package
forecast(ausbeer, h=4)
autoplot(forecast(ausbeer, h=4)) # automatic ETS algorithm

