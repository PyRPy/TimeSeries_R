# Simple explanation of dynamic linear models

# plot data 'Nile' --------------------------------------------------------
library(dlm)
plot(Nile, type='o', col = c("darkgrey"),
     xlab = "", ylab = "Level")

length(Nile)
# fit dlm -----------------------------------------------------------------
# var(Nile)
mod1 <- dlmModPoly(order = 1, dV = 15100, dW = 755)
NileFilt1 <- dlmFilter(Nile, mod1)
lines(dropFirst(NileFilt1$m), lty = "longdash")

mod2 <- dlmModPoly(order = 1, dV = 15100, dW = 7550)
NileFilt2 <- dlmFilter(Nile, mod2)
lines(dropFirst(NileFilt2$m), lty = "dotdash")

leg <- c("data", paste("filtered,  W/V =",
                       format(c(W(mod1) / V(mod1),
                                W(mod2) / V(mod2)))))
legend("bottomright", legend = leg,
       col=c("darkgrey", "black", "black"),
       lty = c("solid", "longdash", "dotdash"),
       pch = c(1, NA, NA), bty = "n")
