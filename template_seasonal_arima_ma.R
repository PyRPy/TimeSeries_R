
library(astsa)
sarima(log(AirPassengers),0,1,1,0,1,1,12)
(dog <- sarima(log(AirPassengers),0,1,1,0,1,1,12))
summary(dog$fit)  # fit has all the returned arima() values
plot(resid(dog$fit))  # plot the innovations (residuals) 
sarima(log(AirPassengers),0,1,1,0,1,1,12,details=FALSE)$BIC  # print model BIC only
# fixed parameters
x = arima.sim(list(order=c(2,0,0), ar=c(0,-.9)), n=200) + 50 
sarima(x, 2,0,0, fixed=c(0,NA,NA))


# ref https://online.stat.psu.edu/stat510/lesson/2/2.1
# ref https://people.duke.edu/~rnau/411arim3.htm

acfma1=ARMAacf(ma=c(0.7), lag.max=10) # 10 lags of ACF for MA(1) with theta1 = 0.7
lags=0:10 #creates a variable named lags that ranges from 0 to 10.
plot(lags,acfma1,xlim=c(1,10), ylab="r",type="h", main = "ACF for MA(1) with theta1 = 0.7")
abline(h=0) #adds a horizontal axis to the plot 
acfma1
