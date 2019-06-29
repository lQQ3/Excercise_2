# Libraries
install.packages('deSolve', repos='http://probability.ca/cran')
library('deSolve')

# Declare and prime variables
N = 1000000 # Total number of people
S <- N - 1 # Rate of Susceptibility
I <- 1 # Total number of Infected people
R <- 0 # Total number of Recovered people
b <- 0.5
m <- 0.1
t <- 0

# Make vectors of S, I & R
while (t<100) {
  S <- c(S - (b * S * I) / N)
  I < - c(I + b * ((S * I)/N))
  R <- c(m * I)
  t=t+1 

}

# Range set
graph_range <- range(0, N)

# Define colors
plot_colors <- c("blue", "red", "forestgreen")

# Start PNG device driver to save output to figure.png


# Plot it
# Plot susceptible with o's and blue lines
plot(S, type = "o", col = "blue", ylim=graph_range) #, axes = FALSE, ann = FALSE)

# X Axis
#axis(1, at= lab=c("0", "25", "50", "75", "100"))

# Box it
# box()

# Graph infectious with red dashed lines and square points
lines(I, type = "x", pch=22, lty=2, col = "red")


lines(R, type = "o", pch=22, lty=2, col = "green")


# Style it
# Title
title(main="SIR Infection Model", col.main="blue")

# Label the x & y axes in forest green
title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Total Number of People", col.lab=rbg(0,0.5,0))

# Legend
legend(1, graph_range[2], c("S", "I"), cex=0.8, col=c("blue", "red" ), pch=21:22, lty=1:2);