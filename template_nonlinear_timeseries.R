# Nonlinear time series ---------------------------------------------------

# code from https://robjhyndman.com/hyndsight/nlts/

simnlts <- function(n, alpha, beta, r, sigma, gamma, burnin=100)
{
  # Generate noise
  e <- rnorm(n+burnin, 0, sigma)
  # Create space for y
  y <- numeric(n+burnin)
  # Generate time series
  for(i in 2:(n+burnin))
  {
    if(y[i-1] <= r)
      y[i] <- alpha*y[i-1] + e[i]
    else
      y[i] <- beta*y[i-1] + gamma*e[i]
  }
  # Throw away first burnin values
  y <- ts(y[-(1:burnin)])
  # Return result
  return(y)
}

fitnlts <- function(x)
{
  ss <- function(par,x)
  {
    alpha <- par[1]
    beta <- par[2]
    r <- par[3]
    n <- length(x)
    # Check that each regime has at least 10% of observations
    if(sum(x<=r) < n/10 | sum(x>r) < n/10)
      return(1e20)
    e1 <- x[2:n] - alpha*x[1:(n-1)]
    e2 <- x[2:n] - beta*x[1:(n-1)]
    regime1 <- (x[1:(n-1)] <= r)
    e <- e1*(regime1) + e2*(!regime1)
    return(sum(e^2))
  }
  fit <- optim(c(0,0,mean(x)),ss,x=x,control=list(maxit=1000))
  if(fit$convergence > 0)
    return(rep(NA,3))
  else
    return(c(alpha=fit$par[1],beta=fit$par[2],r=fit$par[3]))
}

y <- simnlts(100, 0.5, -1.8, -1, 1, 2)
fitnlts(y)

#     alpha       beta          r 
# 0.4416415 -1.6761481 -1.1161421 