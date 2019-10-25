# Outliers Detection and Intervention Analysis
# https://www.r-bloggers.com/outliers-detection-and-intervention-analysis/

# Outliers Analysis -------------------------------------------------------
# Step function
tc <- rep(0, 50)
tc[20] <- 1
ls <- filter(tc, filter = 1, method = "recursive")
plot(ls, main = "Level Shift - TC delta = 1", type = "s")

# Pulse function
ao <- filter(tc, filter = 0, method = "recursive")
plot(ao, main = "Additive Outlier - TC delta = 0", type = "s")

# Transient change function
tc_0_4 <- filter(tc, filter = 0.4, method = "recursive")
tc_0_8 <- filter(tc, filter = 0.8, method = "recursive")
plot(tc_0_4, main = "TC delta = 0.4")
plot(tc_0_8, main = "TC delta = 0.8")

# libraries
library(tsoutliers)
library(TSA)
library(lmtest)
library(astsa)


# Analysis ----------------------------------------------------------------

url <- "https://www.openintro.org/stat/data/arbuthnot.csv"
abhutondot <- read.csv(url, header=TRUE)
write.csv(abhutondot, "abhutondot.csv")
head(abhutondot)

boys_ts <- ts(abhutondot$boys, frequency=1, start = abhutondot$year[1])
girls_ts <- ts(abhutondot$girls, frequency=1, start = abhutondot$year[1])

delta_ts <- boys_ts - girls_ts
excess_ts <- delta_ts/girls_ts
plot(excess_ts)

# tsoulier package
outliers_excess_ts <- tso(excess_ts, 
                          types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts

plot(outliers_excess_ts)

# time index where the outliers have been detected
(outliers_idx <- outliers_excess_ts$outliers$ind)

# calendar years where the outliers have been detected 
outliers_excess_ts$outliers$time

#length of our time series
n <- length(excess_ts)

# transient change outlier at the same time index as found for our time 
# series
mo_tc <- outliers("TC", outliers_idx)

# transient change effect data is stored into a one-column matrix, tc
tc <- outliers.effects(mo_tc, n)

# converting to a number
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])

# obtaining the transient change data with same magnitude as determined 
# by the tso() function
tc_effect <- coefhat*tc

# definining a time series for the transient change data
tc_effect_ts <- ts(tc_effect, frequency = frequency(excess_ts), 
                   start = start(excess_ts))

# subtracting the transient change intervention to the original time 
# series, obtaining the pre-intervention time series
excess_wo_ts <- excess_ts - tc_effect_ts

# plot of the original, the pre-intervention and transient change time 
# series 
plot(cbind(excess_ts, excess_wo_ts, tc_effect_ts))


# highlight the difference between the original time series and the 
# pre-intervention one.
plot(excess_ts, type ='b', ylab = "excess birth ratio")
lines(excess_wo_ts, col = 'red', lty = 3, type ='b')

# check on the residuals of the pre-intervention time series
sarima(excess_wo_ts, p=0, d=0, q=0)


# transient change outlier by arimax() function ---------------------------


arimax_model <- arimax(excess_ts,
                       order = c(0,0,0),
                       seasonal = list(order = c(1,0,0), period = 10),
                       xtransf = data.frame(I1 = (1*(seq(excess_ts) == outliers_idx))),                                                                                      
                       transfer = list(c(1,0)),
                       method='ML')

summary(arimax_model)

# significance of the coefficients is then verified
coeftest(arimax_model)

# check on the residuals of the pre-intervention time series confirms 
# validity of the ARIMA(0,0,0)(1,0,0)[10] 
sarima(excess_wo_ts, p=0, d=0, q=0, P=1, D=0, Q=0, S=10)

# plot the original time series against the fitted one
plot(excess_ts)
lines(fitted(arimax_model), col = 'blue')

# pulse intervention variable
int_var <- 1*(seq(excess_ts) == outliers_idx)


# transient change intervention variable obtained by filtering the pulse 
# according to the definition of transient change and parameters obtained 
# by the ARIMAX model 
tc_effect_arimax <- filter(int_var, filter = coef(arimax_model)["I1-AR1"], 
                           method = "rec", sides = 1) * coef(arimax_model)["I1-MA0"]

# defining the time series for the intervention effect
tc_effect_arimax_ts <- ts(tc_effect_arimax, frequency = frequency(excess_ts), 
                          start = start(excess_ts))

# compare two transient change effects
# comparing transient change effect resulting by ARIMAX (red) with the 
# tso() one (blue)
plot(tc_effect_arimax_ts, col ='red', type='l', ylab = "transient change")
lines(tc_effect_ts, col ='blue', lty = 3)

