# Pharmacokinetic two compartment model
# Shawn Rice
# 5/18/18

# Formulae
# dC{L}/dt=k{FL}C{F}-k{LF}C{L}-e{e}C{L}
# dC{F}/dt=k{LF}C{L}-k{FL}C{F}

install.packages("FME")
library("FME")
twocomp <- function (time, y, parms, ...) {
  with(as.list(c(parms, y)), {
    dCL <- kFL * CF - kLF * CL - ke * CL  # concentration in liver
    dCF <-    kLF * CL  - kFL * CF        # concentration in fat
    list(c(dCL, dCF))
  })
}

dat <- data.frame(
  time = seq(0, 28, 4),
  CL = c(1.31, 0.61, 0.49, 0.41, 0.20, 0.12, 0.16, 0.21),
  CF = c(1e-03, 0.041, 0.05, 0.039, 0.031, 0.025, 0.017, 0.012)
)
parms <- c(ke = 0.2, kFL = 0.1, kLF = 0.05)
times <- seq(0, 40, length=200)
y0 <- c(CL = 1, CF = 0)
out1 <- ode(y0, times, twocomp, parms)
plot(out1, obs = dat)

cost <- function(p) {
  out <- ode(y0, times, twocomp, p)
  modCost(out, dat, weight="none") # remember to try "std" & "mean"
}

parms <- c(ke = 0.2, kFL = 0.1, kLF = 0.05)
fit <- modFit(f = cost, p = parms)
summary(fit)

out1 <- ode(y0, times, twocomp, parms)
out2 <- ode(y0, times, twocomp, coef(fit))
plot(out1, out2, obs=dat, obspar=list(pch=16, col="red"))

cost <- function(p, data, ...) {
  yy <- p[c("CL", "CF")]
  pp <- p[c("ke", "kFL", "kLF")]
  out <- ode(yy, times, twocomp, pp)
  modCost(out, data, ...)
}

parms <- c(CL = 1.2, CF = 0.001, ke = 0.2, kFL = 0.1, kLF = 0.05)
fit <- modFit(f = cost, p = parms, data = dat, weight = "std", 
              lower = rep(0, 5), upper = c(2,2,1,1,1), method = "Marq")

y0 <- coef(fit)[c("CL", "CF")]
pp <- coef(fit)[c("ke", "kFL", "kLF")]
out3 <- ode(y0, times, twocomp, pp)
plot(out1, out2, out3, obs=dat, col=c("grey", "blue", "red"), lty = 1)

summary(fit)

