# kalman filter -----------------------------------------------------------

# ref Practical Timeseries Analysis

## rocket will take 100 time steps
ts.length <- 100
# the acceleration will drive the motion
a <- rep(0.5, ts.length)
a
# position and velocity start a 0
x <- rep(0, ts.length)
v <- rep(0, ts.length)

for (ts in 2:ts.length){
  x[ts] <- v[ts -1] + x[ts - 1] + 1/2 * a[ts - 1]^2
  x[ts] <- x[ts] + rnorm(1, sd = 20)
  v[ts] <- v[ts - 1] + 2 * a[ts - 1]
}

par(mfrow = c(3, 1))
plot(x, main = "position", type="l")
plot(v, main = "velocity", type="l")
plot(a, main = "accelaration", type="l")

par(mfrow = c(1, 1))
# measurement from sensor
z <- x + rnorm(ts.length, sd=300)
plot(x, ylim = range(c(x, z)))
lines(z, col='red')

# apply kalman filter
kalman.motion <- function(z, Q, R, A, H){
  dimState = dim(Q)[1]
  xhatminus <- array(rep(0, ts.length * dimState), c(ts.length, dimState))
  xhat <- array(rep(0, ts.length * dimState), c(ts.length, dimState))
  Pminus <- array(rep(0, ts.length * dimState * dimState), 
                  c(ts.length, dimState, dimState))
  P <- array(rep(0, ts.length * dimState * dimState),
             c(ts.length, dimState, dimState))
  K <- array(rep(0, ts.length * dimState),
             c(ts.length, dimState))
  
  # initial guesses = starting at 0 for all metrics
  xhat[1, ] <- rep(0, dimState)
  P[1, , ] <- diag(dimState)
  
  for (k in 2:ts.length){
    # time update
    xhatminus[k, ] <- A %*% matrix(xhat[k-1, ])
    Pminus[k, , ] <- A %*% P[k-1, , ] %*% t(A) + Q
    K[k, ] <- Pminus[k, , ] %*% H %*% solve(t(H) %*% Pminus[k, , ] %*% H + R)
    xhat[k, ] <- xhatminus[k, ] + K[k, ] %*% (z[k] - t(H) %*% xhatminus[k, ])
    P[k, , ] <- (diag(dimState) - K[k, ] %*% t(H)) %*% Pminus[k, , ]
  }
  
  # we return both the forecast and the smoothed value
  return(list(xhat = xhat, xhatminus = xhatminus))
}

# noise parameters
R <- 10^2
Q <- 10

# dynamical paramters
A <- matrix(1) # x_t = A * x_t-1
H <- matrix(1) # y_t = H* x_t

# run the data through the kalman filtering method
xhat <- kalman.motion(z, diag(1)*Q, R, A, H)[[1]]
lines(xhat, col='blue')
