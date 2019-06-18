# Chapter 2 Time series graphics
# https://otexts.com/fpp2/ts-objects.html
library(fpp2)
# 2.1 ts objects
y <- ts(c(123, 39, 78, 52, 110), start=2012)

z <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

y <- ts(z, start=2003, frequency = 12)
plot(y)

x <- c(1:72)
x.ts <- ts(x, start = 2012, frequency = 1440)
autoplot(x.ts)
x.ts

# 2.2 time plots
autoplot(melsyd[, "Economy.Class"]) +
  ggtitle("Economy class : Melbournce - Sydney")
  xlab("Year")+
  ylab("Thousands")

head(melsyd)


autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

# 2.3 time series patterns
# 2.4 seasonal plots
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
               ylab("$ million") +
               ggtitle("Seasonal plot : antidiabetic drug sales")

ggseasonplot(a10, polar = TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot : antidiabetic drug sales")

# seasonal subseries plots
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot : antidiabetic drug sales")

autoplot(elecdemand[, c("Demand", "Temperature")], facets = TRUE) +
  xlab("Year : 2014") +
  ylab("") +
  ggtitle("Half-hourly electricity demand : Victoria, Austrilia")

class(elecdemand)               

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)")+
  xlab("Temperature (C)")

# scatterplot matrices
autoplot(visnights[, 1:5], facets = TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

# pairs
GGally::ggpairs(as.data.frame(visnights[, 1:5]))

# 2.7 lag plots
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

# 2.8 autocorrelation
ggAcf(beer2)

aelec <- window(elec, start=1980)
autoplot(aelec)+
  xlab("year") +
  ylab("GWh")

ggAcf(aelec, lag=48)

# 2.9 white noise
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) +
  ggtitle("White noise")

ggAcf(y)
