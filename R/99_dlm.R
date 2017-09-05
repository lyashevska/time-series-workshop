##-----------------------------
## Fit and plot a state space model fit by the Kalman filter
## CM: 05/10/2017
## EPA tipping points workshop
##-----------------------------

## load the dlm library
library(dlm)

## read in the data
wren <- read.csv("../data/house-wren.csv")

year <- wren$sample_year
y <- log(wren$population_untransformed)
n <- length(y)
ylab <- "Index"

## plot the data
plot(year, y, type = "o", col = "darkgrey", ylab = ylab, pch = 19, bty = "l")

## need to estimate the parameters of the non-zero mean AR(1)
build <- function(theta){
    ar1 <- -1 + 2 * plogis(theta[1]) ## AR(1) bounded [-1,1]
    sdp <- exp(theta[2]) ## process error standard deviation
    sde <- exp(theta[3]) ## measurement error standard deviation
    mod <- dlmModPoly(1, dV = sde^2, dW = 0) +
        dlmModARMA(ar = ar1, ma = NULL, sigma2 = sdp^2)
    mod$C0[2,2] <- solve(1 - ar1^2) * sdp^2
    return(mod)
}

fit <- dlmMLE(y = y, parm = c(0, rep(log(0.1), 2)), build = build, hessian=T, method = "Nelder-Mead", control = list(trace = 1))

## INSPECT RESIDS!!

## filter
build.hat <- build(fit$par)
filt <- dlmFilter(y, build.hat)
smooth <- dlmSmooth(y, build.hat)
## get asymptotic 95% CI on smoothed state
se <- sqrt(unlist(
    lapply(dlmSvd2var(smooth$U.S, smooth$D.S), FUN = function(z){z[1,1] + z[2,2] + 2 * z[1,2]})
))
se <- se[-1]
##
plot(year, y, type = "o", col = "darkgrey", ylab = ylab, pch = 19, bty = "l")
smooth.hat <- dropFirst(rowSums(smooth$s))
lines(year, smooth.hat, col="blue")
lines(year, smooth.hat + 2 * se, lty = 2, col="blue")
lines(year, smooth.hat - 2 * se, lty = 2, col="blue")
