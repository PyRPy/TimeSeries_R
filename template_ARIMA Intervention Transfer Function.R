# ARIMA Intervention Transfer Function - How to Visualize the Effect

# The Data ----------------------------------------------------------------

cds <- structure(c(2580L, 2263L, 3679L, 3461L, 3645L, 3716L, 3955L, 3362L,
                   2637L, 2524L, 2084L, 2031L, 2256L, 2401L, 3253L, 2881L,
                   2555L, 2585L, 3015L, 2608L, 3676L, 5763L, 4626L, 3848L,
                   4523L, 4186L, 4070L, 4000L, 3498L),
                 .Dim=c(29L, 1L),
                 .Dimnames=list(NULL, "CD"),
                 .Tsp=c(2012, 2014.33333333333, 12), class="ts")

plot(cds, type="b")


# The methodology ---------------------------------------------------------

# pre-intervention series (up until October 2013) 
pre <- window(cds, start=c(2012, 01), end=c(2013, 09))
mod.pre <- auto.arima(log(pre))
mod.pre

#  the pulse response was chosen below, with T = Oct 2013
library(TSA)
mod.arimax <- arimax(log(cds), order=c(1, 0, 0),
                     seasonal=list(order=c(0, 0, 0), frequency=12),
                     include.mean=TRUE,
                     xtransf=data.frame(Oct13=1 * (seq(cds) == 22)),
                     transfer=list(c(1, 1)))
mod.arimax

# check residuals
tsdiag(mod.arimax)

# plot of fitted and actuals
plot(fitted(mod.arimax), col="red", type="b")
lines(window(log(cds), start=c(2012, 02)), type="b")

# the residuals from the two parametrizations
fit <- arimax(log(cds), order=c(1, 0, 0),
              xtransf=
                data.frame(Oct13a=1 * (seq_along(cds) == 22),
                           Oct13b=1 * (seq_along(cds) == 22)),
              transfer=list(c(0, 0), c(1, 0)))

plot(resid(fit), type="b")

# Then, from this fit
mod.arimax <- arimax(log(cds), order=c(1, 0, 0),
                     seasonal=list(order=c(0, 0, 0), frequency=12),
                     include.mean=TRUE,
                     xtransf=data.frame(Oct13=1 * (seq(cds) == 22)),
                     transfer=list(c(1, 1))) 

mod.arimax
plot(resid(mod.arimax), type="b")


# 3 Answers ---------------------------------------------------------------
require(TSA)
cds <- structure(c(2580L, 2263L, 3679L, 3461L, 3645L, 3716L, 3955L, 3362L,
                   2637L, 2524L, 2084L, 2031L, 2256L, 2401L, 3253L, 2881L,
                   2555L, 2585L, 3015L, 2608L, 3676L, 5763L, 4626L, 3848L,
                   4523L, 4186L, 4070L, 4000L, 3498L),
                 .Dim = c(29L, 1L),
                 .Dimnames = list(NULL, "CD"),
                 .Tsp = c(2012, 2014.33333333333, 12),
                 class = "ts")

fit <- arimax(log(cds), order = c(1, 0, 0), 
              xtransf = data.frame(Oct13a = 1 * (seq_along(cds) == 22), 
                                   Oct13b = 1 * (seq_along(cds) == 22)),
              transfer = list(c(0, 0), c(1, 0)))
fit

# test the significance of each intervention
require(lmtest)
coeftest(fit)

# intervention effect can be quantified as follows
intv.effect <- 1 * (seq_along(cds) == 22)
intv.effect <- ts(
  intv.effect * 0.1251 + 
    filter(intv.effect, filter = 0.9231, method = "rec", sides = 1) * 0.4332)
intv.effect <- exp(intv.effect)
tsp(intv.effect) <- tsp(cds)

# plot the effect of the intervention
plot(100 * (intv.effect - 1), type = "h", main = "Total intervention effect")

# estimated increases quantified at each time point caused by the the 
# intervention in October 2013
window(100 * (intv.effect - 1), start = c(2013, 10))

# The interventions are a pulse plus a transitory change with parameter 
# 0.9231 and can be built as follows
xreg <- cbind(
  I1 = 1 * (seq_along(cds) == 22), 
  I2 = filter(1 * (seq_along(cds) == 22), filter = 0.9231, method = "rec", 
              sides = 1))
arima(log(cds), order = c(1, 0, 0), xreg = xreg)

# These interventions are equivalent to an additive outlier (AO) and a 
# transitory change (TC)
require(tsoutliers)
mo <- outliers(c("AO", "TC"), c(22, 22))
oe <- outliers.effects(mo, length(cds), delta = 0.9231)
arima(log(cds), order = c(1, 0, 0), xreg = oe)

#  the effect of an innovational outlier rather than a pulse plus a 
# transitory change
fit2 <- arimax(log(cds), order=c(1, 0, 0), include.mean = TRUE, 
        xtransf=data.frame(Oct13 = 1 * (seq(cds) == 22)), transfer = list(c(1, 1)))
fit2

# the effect of the intervention can now be quantified as follows
intv.effect <- 1 * (seq_along(cds) == 22)
intv.effect <- ts(intv.effect * 0.4261 + 
                    filter(intv.effect, filter = -0.4429, method = "rec", sides = 1) * 0.3567)
tsp(intv.effect) <- tsp(cds)
window(100 * (exp(intv.effect) - 1), start = c(2013, 10))

plot(100 * (exp(intv.effect) - 1), type = "h", 
     main = "Intervention effect (parameterization 2)")


# Edit 2 ------------------------------------------------------------------
omegas <- seq(0.5, 1, by = 0.01)
aics <- rep(NA, length(omegas))
for (i in seq(along = omegas)) {
  tc <- filter(1 * (seq_along(cds) == 22), filter = omegas[i], method = "rec", 
               sides = 1)
  tc <- ts(tc, start = start(cds), frequency = frequency(cds))
  fit <- arima(log(cds), order = c(1, 0, 0), xreg = tc)
  aics[i] <- AIC(fit)
}
omegas[which.min(aics)]

plot(omegas, aics, main = "AIC for different values of the TC parameter")

# fit the AR(1) model with the intervention as a regressor 
# (with parameter ??2=0.9):

tc <- filter(1 * (seq.int(length(cds) + 12) == 22), filter = 0.9, method = "rec", 
             sides = 1)
tc <- ts(tc, start = start(cds), frequency = frequency(cds))
fit <- arima(window(log(cds), end = c(2013, 10)), order = c(1, 0, 0), 
             xreg = window(tc, end = c(2013, 10)))

# forecasts can be obtained and displayed as follows:

p <- predict(fit, n.ahead = 19, newxreg = window(tc, start = c(2013, 11)))

plot(cbind(window(cds, end = c(2013, 10)), exp(p$pred)), plot.type = "single", 
     ylab = "", type = "n")
lines(window(cds, end = c(2013, 10)), type = "b")
lines(window(cds, start = c(2013, 10)), col = "gray", lty = 2, type = "b")
lines(exp(p$pred), type = "b", col = "blue")
legend("topleft",
       legend = c("observed before the intervention",
                  "observed after the intervention", "forecasts"),
       lty = rep(1, 3), col = c("black", "gray", "blue"), bty = "n")

# 95% confidence intervals can be added to the previous plot as follows:
lines(exp(p$pred + 1.96 * p$se), lty = 2, col = "red")
lines(exp(p$pred - 1.96 * p$se), lty = 2, col = "red")


# using tsouliters package ------------------------------------------------


# tsoutliers package can be thought of as an automatic intervention detection
cds<- structure(c(2580L, 2263L, 3679L, 3461L, 3645L, 3716L, 3955L, 
                  3362L, 2637L, 2524L, 2084L, 2031L, 2256L, 2401L, 3253L, 2881L, 
                  2555L, 2585L, 3015L, 2608L, 3676L, 5763L, 4626L, 3848L, 4523L, 
                  4186L, 4070L, 4000L, 3498L), .Dim = c(29L, 1L), .Dimnames = list(
                    NULL, "CD"), .Tsp = c(2012, 2014.33333333333, 12), class = "ts")
arimatr <- tsoutliers::tso(cds,args.tsmethod=list(d=0,D=0))
plot(arimatr)
arimatr



# https://stats.stackexchange.com/questions/108374/arima-intervention-transfer-function-how-to-visualize-the-effect