# Chapter 8. Generating and Selecting Features for a Time Series
# ref https://cran.r-project.org/web/packages/tsfeatures/vignettes/tsfeatures.html
library(tsfeatures)
mylist <- list(sunspot.year, AirPassengers)
tsfeatures(mylist)

# check additional features
is.monthly <- function(x){
  frequency(x) == 12
}

tsfeatures(mylist, features = "is.monthly")


# stl features ------------------------------------------------------------
plot(AirPassengers, type="l")
stl_features(AirPassengers)


# acf features ------------------------------------------------------------

acf_features(AirPassengers)


# heterogeneity -----------------------------------------------------------

heterogeneity(AirPassengers)


# 2015 papter -------------------------------------------------------------

library(dplyr)
library(fpp2)
yahoo <- yahoo_data()
# plot(yahoo[1], type="l")
str(yahoo)
# autoplot(yahoo)
# plot.ts(yahoo)

hwl <- bind_cols(
        tsfeatures(yahoo,
                   c("acf_features", "entropy", "lumpiness",
                     "flat_spots", "crossing_points")),
        tsfeatures(yahoo, "stl_features", s.window='periodic', robust=TRUE),
        tsfeatures(yahoo, "max_kl_shift", width=48),
        tsfeatures(yahoo, 
                   c("mean", "var"), scale=FALSE, na.rm=TRUE),
        tsfeatures(yahoo,
                   c("max_level_shift", "max_var_shift"), trim = TRUE)) %>% 
  select(mean, var, x_acf1, trend, linearity, curvature,
         seasonal_strength, peak, trough,
         entropy, lumpiness, spike, max_level_shift, max_var_shift, flat_spots,
         crossing_points, max_kl_shift, time_kl_shift)
       

# extract features
library(ggplot2)
hwl_pca <- hwl %>%
  na.omit() %>% 
  prcomp(scale=TRUE)

hwl_pca$x %>% 
  as_tibble() %>% 
  ggplot(aes(x = PC1, y=PC2)) +
  geom_point()

# plot data
plot(yahoo["dat01.S1"])
dat <- yahoo["dat0.S1"]
plot.ts(dat)
str(yahoo)
yahoo[1:5]["dat0.S1"]


# IJF 2017 papter ---------------------------------------------------------

library(tsfeatures)
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)

# get data set
M3data <- purrr::map(Mcomp::M3,
  function(x){
    tspx <- tsp(x$x)
    ts(c(x$x, x$xx), start = tspx[1], frequency = tspx[3])
  })

khs_stl <- function(x,...){
  lambda <- BoxCox.lambda(x, lower = 0, upper = 1, method = 'loglik')
  y <- BoxCox(x, lambda)
  c(stl_features(y, s.window='periodic', robust=TRUE, ...), lambda=lambda)
}

khs <- bind_cols(
  tsfeatures(M3data, c("frequency", "entropy")),
  tsfeatures(M3data, "khs_stl", scale = FALSE)) %>% 
  select(frequency, entropy, trend, seasonal_strength, e_acf1, lambda) %>% 
  replace_na(list(seasonal_strength=0)) %>% 
  rename(
    Frequency = frequency,
    Entropy = entropy,
    Trend = trend,
    Season = seasonal_strength,
    ACF1 = e_acf1,
    Lambda = lambda) %>% 
  mutate(Period = as.factor(Frequency))

# plot the results
khs %>%
  select(Period, Entropy, Trend, Season, ACF1, Lambda) %>% 
  GGally::ggpairs()
 
# extract space components
khs_pca <- khs %>%
  select(-Period) %>% 
  prcomp(scale = TRUE)

khs_pca$x %>% 
  as_tibble() %>% 
  bind_cols(Period = khs$Period) %>% 
  ggplot(aes(x=PC1, y=PC2)) +
  geom_point(aes(col=Period))
