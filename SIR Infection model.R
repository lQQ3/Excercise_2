
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

# Make S, I & R lists
suscept = list("S")
infect = list("I")
recover = list("R")

# Make a function called 'infection' and the arguments S, I, R & N
infection <- function(S, I, R, N)
  # These are the formulae
  while (t < 100) {
    S <- S-(b*S*I)/N
    I <- I + ((b * S * I)/N) - R
    R <- m * I
    
    # Append the outputs of S, I & R to the appropriate vector
    suscept <- c(S)
    infect <- c(I)
    recover <- c(R)
    
    # Increment t
    t = t+1
  }

# Call to the function and args                    
infection(S,I,R,N)

# Define vectors


# Make the graph and title it
plot(infection(S, I, R, N), type="o", col="blue")