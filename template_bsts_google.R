# Bayesian structural time series model

# # read the data ---------------------------------------------------------
# https://openei.org/datasets/files/961/pub/RESIDENTIAL_LOAD_DATA_E_PLUS_OUTPUT/
elec <- read.csv("OHare.csv")
head(elec)

require(bsts)
n = colnames(elec)[9]

par(mfrow = c(2, 1))
plot(elec[[n]][1:4000]) # long period - hourly
plot(elec[[n]][1:96]) # short period
par(mfrow = c(1, 1))


# # inspect possible seasonal patterns ------------------------------------

ss <- AddLocalLinearTrend(list(), elec[[n]])
ss <- AddSeasonal(ss, elec[[n]], nseasons = 24, season.duration = 1)
ss <- AddSeasonal(ss, elec[[n]], nseasons = 7, season.duration = 24)



# run the model -----------------------------------------------------------

model1 <- bsts(elec[[n]], state.specification = ss,
               niter = 100)

plot(model1, xlim=c(1800, 1900))
plot(model1, "seasonal", nseasons = 7, season.duration = 24)


# make predictions --------------------------------------------------------

pred <- predict(model1, horizon = 24, quantiles = c(0.05, 0.95))
plot(pred, plot.original = 72)

# not very impressive !