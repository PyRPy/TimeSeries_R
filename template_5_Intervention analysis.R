# 5 Intervention analysis

# 5.1 A known intervention ------------------------------------------------

library(Kendall)
data("GuelphP")
head(GuelphP)

plot(GuelphP, type="b", ylab="P concentration, mg l-1",
     main="Speed river water quality")
abline(v = 1974 + 2/12, col="red", lwd=2)
text(x = 1974 + 2/12, y=1, "known intervention", col="red", pos = 4)

# Split the series at February 1974 and compare the means, medians 
# or variances.
guelph.1 <- na.omit(as.vector(window(GuelphP, start=NULL, end=1974 + 1/12)))
guelph.2 <- na.omit(as.vector(window(GuelphP, start=1974 + 2/12, end=NULL)))

mean(guelph.1)
mean(guelph.2)

median(guelph.1)
median(guelph.2)

sd(guelph.1)
sd(guelph.2)

