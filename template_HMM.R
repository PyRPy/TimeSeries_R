# template for HMM in code
# ref : Practical time series analysis

# create a time series data -----------------------------------------------

# set seed
set.seed(123)

# set four states
bull_mu <- 0.1
bull_sd <- 0.1

neutral_mu <- 0.02
neutral_sd <- 0.08

bear_mu <- -0.03
bear_sd <- 0.2

panic_mu <- 0.1
panic_sd <- 0.3

# form vectors and organize data
mus <- c(bull_mu, neutral_mu, bear_mu, panic_mu)
sds <- c(bull_sd, neutral_sd, bear_sd, panic_sd)

# set constants relatd to time series
NUM.PERIODS <- 10
SMALLIST.PERIOD <- 20
LONGEST.PERIOD <- 40

# each day count indicating one 'run' or one state of the market
days <- sample(SMALLIST.PERIOD:LONGEST.PERIOD, NUM.PERIODS, replace = TRUE)

returns <- numeric()
true.mean <- numeric()
for (d in days){
  idx = sample(1:4, 1, prob = c(0.2, 0.6, 0.18, 0.02))
  returns <- c(returns, rnorm(d, mean = mus[idx], sd=sds[idx]))
  true.mean = c(true.mean, rep(mus[idx], d))
}

# check the data
table(true.mean)


# fit the model -----------------------------------------------------------

require(depmixS4)
hmm.model <- depmix(returns ~ 1, family = gaussian(),
                    nstates = 4, data=data.frame(returns=returns))

model.fit <- fit(hmm.model)
post_probs <- posterior(model.fit)


# plot the states vs measured values --------------------------------------

plot(returns, type="l", lwd=3, col=1,
     yaxt = "n", xaxt="n", xlab = "", ylab = "",
     ylim = c(-0.6, 0.6))

rect_background <- function(i) {
  rect(i, -0.6, (i+ 1), 0.6, 
       col = rgb(0.0, 0.0, 0.0, alpha = (0.2 * post_probs$state[i + 1])),
       border = NA)
}

# mapping states into data sections

lapply(0:length(returns) - 1, rect_background)


attr(model.fit, "response")
