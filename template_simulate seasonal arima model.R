# how to simulate seasonal arima model in R
# https://stackoverflow.com/questions/6948440/r-generate-a-seasonal-arima-time-series-model-using-parameters-of-existing-data
# arima.sim() # not working ?

# install.packages("devtools")

# simulate seasonal arima model -------------------------------------------

# devtools::install_github("smac-group/gmwm")
library(gmwm)
# Set seed for reproducibility
set.seed(1019)


# Generate a SARIMA(1,0,1)(1,0,0)[12] 
mod <- SARIMA(ar=0, i = 0, ma=.8, sar = 0, si = 0, sma = .7, s = 4, sigma2 = 1)

# Generate the data
xt <- gen_gts(250, mod)
library(astsa)
acf2(xt)

# Try to recover parameters
arima(xt, order = c(0,0,1), seasonal = list(order = c(0,0,1), period = 4), include.mean = FALSE)

# verify the model
sarima(xt, 0,0,1, 0,0,1, 4)
write.csv(xt, "dat_sim.csv")
plot(xt)
