library(deSolve)
chaos<-function(t, state, parameters){
  with(as.list(c(state)), {
    dx <- -8/3 * x + y * z
    dy <- -10 * (y - z)
    dz <- - x *y + 28 * y - z
    list(c(dx, dy, dz))
  })
}
state <- c(x = 1, y = 1, z = 1)
times <- seq(0, 100, 0.01)
out <-vode(state, times, chaos, 0)

plot(out[,"x"], out[,"y"], type = "l", main = "Lorenz Butterfly")