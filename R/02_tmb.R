## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(size="footnotesize")
## set the working directory
##setwd('../tex'); 

## ----echo=FALSE, results='asis', fig.height = 5--------------------------
curve(2 + 3 * x - x^2, from = 0, to = 3, ylab = "y", xlab = "x", 
      cex.axis = 1.5, cex.lab = 2, bty = "l")
abline(v = 3/2, col = "grey")

## ----warning = FALSE-----------------------------------------------------
fx <- function(x){
  y <- 2 + 3 * x -x^2
  ## return -y as optim automatically does 
  ## minimization
  return(-y)
}

nlminb(objective = fx, start = 0)


## ------------------------------------------------------------------------
dydx <- function(x){
  dy <- 3 - 2 * x
  return(-dy)
}

nlminb(objective = fx, gradient = dydx, start = 0)


## ------------------------------------------------------------------------
## generate some data - 10,000 observations
set.seed(101)
y <- rnorm(1e4, mean = 8)

## log-likelihood function
fmu <- function(mu){
  ll <- dnorm(y, mean = mu, sd = 2, log = TRUE)
  return(-sum(ll))
}


## ----fig.height = 5, fig.width = 6, fig.cap = "Log-likelihood of $\\mathbf{y}$ over $\\mu$ with the maximum likelihood value of $\\mu$ shown as a vertical grey line."----
fmu2 <- function(mu){
  ll <- dnorm(y, mean = mu, sd = 2, log = TRUE)
  return(sum(ll))
}

h <- Vectorize(fmu2)
curve(h, from = 5, to = 12, ylab = "ln L(y | mu)", xlab = expression(mu), 
      cex.axis = 1.5, , cex.lab = 2, bty = "l")
abline(v = mean(y), col = "grey")

## ------------------------------------------------------------------------
nlminb(objective = fmu, start = 5)

## ------------------------------------------------------------------------

dfmu <- function(mu){
  dll <- sum(2 * y / 8 - 2 * mu /8)
  return(-dll)
}

nlminb(objective = fmu, gradient = dfmu, start = 5)

## ------------------------------------------------------------------------
## load the TMB library
library(TMB)


## ----eval = FALSE, warning = FALSE---------------------------------------
## 
## ## step takes a while (on my machine anyway)
## compile("unknown_mean.cpp")
## 

## ------------------------------------------------------------------------
## load the function
dyn.load(dynlib("unknown_mean"))

## ------------------------------------------------------------------------
obj <- MakeADFun(
         data = list(y = y), 
         parameters = list(mu = 5),
         DLL = "unknown_mean",
         silent = TRUE)

## compare the log-likelihoods
obj$fn(5); fmu(5)
## compare the gradients
obj$gr(5); dfmu(5)
## quite impressive!

## ------------------------------------------------------------------------

(opt <- nlminb(objective = obj$fn, gradient = obj$gr, start = obj$par))


## ------------------------------------------------------------------------

(rep <- sdreport(obj))


## ----eval = FALSE--------------------------------------------------------
## compile("random_intercepts.cpp")
## 

## ------------------------------------------------------------------------

## load the function
dyn.load(dynlib("random_intercepts"))


## ------------------------------------------------------------------------

ngps <- 20

gp.means <- rnorm(ngps, mean = 8, sd = 2)

y <- c(sapply(gp.means, 
              FUN = function(x){rnorm(10, mean = x, sd = 0.5)})
       )
gps <- rep(0:(ngps - 1), each = 10)
library(ggplot2)

dat <- data.frame(gps, y)


## ------------------------------------------------------------------------

obj <- MakeADFun(
         data = list(y = y, gps = gps, ngps = ngps), 
         parameters = 
         list(mu = 5, logsigma = 0.1, logtau = 0.1, u = rep(mean(y), ngps)),
         random = "u",
         DLL = "random_intercepts",
         silent = TRUE)

opt <- nlminb(objective = obj$fn, gradient = obj$gr, start = obj$par)

rep <- sdreport(obj)
rep.summ <- summary(rep)
gp.means <- rep.summ[rownames(rep.summ) == "gp_means",]
row.names(gp.means) <- NULL
pred.dat <- as.data.frame(cbind(gps = 0:(ngps-1), gp.means))
names(pred.dat)[3] <- "SE"
## compare to lme4 in R
library(lme4)
lme.fit <- lmer(y ~ 1 + (1|gps), REML = FALSE)

## TMB values
c(opt$par[1], exp(opt$par[2:3])); -opt$objective
## lme4 values
lme.fit


## ----fig.height = 4, fig.width = 6, fig.cap = "Random intercept data (grey points) and TMB-estimated mixed effect means and approximate confidence intervals."----

theme_set(theme_bw())
ggplot(dat, aes(x = gps, y = y)) + geom_point(col = "slategrey", pch = 1) +
  xlab("Group") +
  geom_point(data = pred.dat, aes(x = gps, y = Estimate), col = "purple", size = 2) +
  geom_errorbar(
    data = pred.dat, aes(x = gps, y = Estimate, 
      ymin = Estimate - 2 * SE, ymax = Estimate + 2 * SE), 
    width = 0.4, col = "purple")


## ------------------------------------------------------------------------

set.seed(1003)
alpha <- 0
sd.proc <- 0.1
T <- 100
I <- rep(1, T)
I[seq(20, T, by = 20)] <- 0

for(i in 2:T){
  alpha[i] <- 
    I[i] * rnorm(1, alpha[i - 1], sd = sd.proc) +
      (1 - I[i]) * rnorm(1, alpha[i - 1], sd = 10 * sd.proc)
}

## add in some measurement noise
y <- rnorm(T, mean = alpha, sd = 0.1)
plot(y)
lines(alpha)


## ------------------------------------------------------------------------
library(dlm)

mod <- dlmModPoly(1)

build <- function(theta){
  mod$V[1,1] <- exp(theta[1])
  mod$W[1,1] <- exp(theta[2])
  return(mod)
}

## fit
kf.fit <- dlmMLE(y = y, parm = log(c(0.1, 0.1)), build = build)

## take a look at the parameters
round(exp(kf.fit$par), 3)

## measurement error sd biased low
## process error sd biased high


## ------------------------------------------------------------------------

ks <- dlmSmooth(y, mod = build(kf.fit$par))
plot(y)
lines(alpha)
lines(dropFirst(ks$s), col = "blue")
## a bit too much wiggle where we don't want it and not
## enough where we do

## uncertainty
## calculate 95% confidence interval
sd <- data.frame(rep(data.frame(NA),1))

for (i in 1:length(ks$U.S)){
  sd[i,] <- sqrt(diag(dlmSvd2var(ks$U.S[[i]], ks$D.S[i,])))
}
# calculate standard error
hwid <- (2 * sd)
hwid <- hwid[-1,] # delets first row
upr<- ks$s[-1] + hwid * -1 # *-1 to make positive
lwr<- ks$s[-1] + hwid * 1 # *-1 to make positive


## ----eval = FALSE--------------------------------------------------------
## 
## library(TMB); library(knitr)
## compile("mixture_timeseries.cpp")
## 

## ----warning = FALSE-----------------------------------------------------

dyn.load(dynlib("mixture_timeseries"))

obj <- MakeADFun(
         data = list(y = y), 
         parameters = 
         list(logsdproc = log(0.1), logsdmeas = log(0.1), alpha = rep(mean(y), T)),
         random = "alpha",
         DLL = "mixture_timeseries",
         silent = TRUE)

(opt <- nlminb(objective = obj$fn, gradient = obj$gr, start = obj$par))

(rep <- sdreport(obj))

rep.sum <- summary(rep)

alpha.hat <- rep.sum[rownames(rep.sum) == "alpha", "Estimate"]
alpha.se <- rep.sum[rownames(rep.sum) == "alpha", "Std. Error"]
pwide.hat <- rep.sum[rownames(rep.sum) == "pwide", "Estimate"]
pwide.hat[1] <- 1


## ----fig.height = 6, fig.width = 7, fig.cap = "Observations (points), true (black) and estimated states from the Kalman filter (blue) and mixture filter (green). Approximate 95\\% confidence instervals are shown as dashed lines. The colour of each point represents the probability it comes from the regular distribution (white) or wide distribution (black)."----

plot(y, pch = 21, bg = grey(pwide.hat))
lines(alpha)
matlines(cbind(alpha.hat + 2 * alpha.se, alpha.hat - 2 * alpha.se), 
         lty = 2, col = "forestgreen")
lines(alpha.hat, col = "forestgreen")

lines(dropFirst(ks$s), col = "blue")
matlines(cbind(lwr, upr), lty = 2, col = "blue")


