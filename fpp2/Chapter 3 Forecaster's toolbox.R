# Chapter 2 Time series graphics
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
