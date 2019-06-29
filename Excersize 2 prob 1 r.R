# e = (1+1/n)^n
# y(t) = a*e^(kt)
# y(t) = value at time 't'
# a = value at start
# k = rate of growth
# t = time
# y(t) = a*(1+(1/(kt))^(kt))
install.packages('deSolve', repos='http://probability.ca/cran')
library(deSolve)

# Create a functrion called 'model' with argument names: 'time', 'y', and 'parms'.
model <- function(time, y, parms){ 
  # Create a list named 'c' with the elements: 'time', 'y', and 'parms'.
  with(as.list(c(y, parms)), { 
    # Create a variable named 'dN' and initializte it to '0'.
    dN <- 0
    # Make a list of 'dN'.
    list (dN)
  })
}
# y == list 'c' where N==0.1.
y <- c(N = 0.1)

# parms == list 'c' where r==0.1 and k==10.
parms <- c(r = 0.1, K = 10)
# times == count from 0 to 100 by 1's.
times <- seq(0, 100, 1) 

# Create a variable named 'out' which will run a set of arguments throough the differential equations solver.
out <- ode(y, times, model, parms)

# Show it
plot(out)