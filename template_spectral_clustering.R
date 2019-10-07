# read brain data
# open data with Rstudio
head(brain)
str(brain)

## load libraries required for analysis
library(Spectrum)
library(plot3D)
library(survival)
library(survminer)
library(scales)

## run Spectrum
r <- Spectrum(brain[[1]])

## do t-sne analysis of results
y <- Rtsne::Rtsne(t(brain[[1]]),dim=3)
scatter3D(y$Y[,1],y$Y[,2],y$Y[,3], phi = 0, #bty = "g", ex = 2,
          ticktype = "detailed", colvar = r$assignments,
          col = gg.col(100), pch=20, cex=2, #type = 'h',
          xlab='Dim1', ylab='Dim2', zlab='Dim3')

## do test
clinicali <- brain[[2]]
clinicali$Death <- as.numeric(as.character(clinicali$Death))
coxFit <- coxph(Surv(time = Time, event = Death) ~ as.factor(r$assignments), data = clinicali, ties = "exact")
coxresults <- summary(coxFit)
print(coxresults$logtest[3])

## survival curve
survival_p <- coxresults$logtest[3]
fsize <- 18
clinicalj <- clinicali
if (0 %in% r$cluster){
  res$cluster <- res$cluster+1
}

clinicalj$cluster <- as.factor(r$assignments)
fit = survfit(Surv(Time,Death)~cluster, data=clinicalj)

## plot
gg <- ggsurvplot(fit, data = clinicalj, legend = 'right', font.x =  fsize, font.y =  fsize, font.tickslab = fsize,
                 palette = gg.col(4),
                 legend.title = 'Cluster', #3 palette = "Dark2",
                 ggtheme = theme_classic2(base_family = "Arial", base_size = fsize))

gg$plot + theme(legend.text = element_text(size = fsize, color = "black")) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = fsize, colour = 'black'),
        axis.text.x = element_text(size = fsize, colour = 'black'),
        axis.title.x = element_text(size = fsize),
        axis.title.y = element_text(size = fsize),
        legend.title = element_text(size = fsize),
        legend.text = element_text(size = fsize))
