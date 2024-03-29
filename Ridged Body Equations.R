yini <- c(1, 0, 0.9)
ridgidode <- function(t, y, parms) {
  dy1 <- -2 * y[2] * y[3]
  dy2 <- 1.25 * y[1] * y[3]
  dy3 <- -0.5 * y[1] * y[2]
  list(c(dy1, dy2, dy3))
}
times <- seq(from = 0, to = 20, by = 0.01)
out <- ode(times = times, y = yini, func = ridgidode, 
           parms = NULL, method = rkMethod("rk45ck"))
head(out, n=3)