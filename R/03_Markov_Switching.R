##-----------------------------
## Fit and plot a Markov switching model
## CM: 05/10/2017
## EPA tipping points workshop
##-----------------------------

## load the library
library(MSwM)

## read in the data
heron <- read.csv("../data/house-heron.csv")

year <- heron$sample_year
y <- log(heron$population_untransformed)
n <- length(y)
ylab <- "Index"

## plot the data
plot(year, y, type = "o", col = "darkgrey", ylab = ylab, pch = 19, bty = "l")

## try some switching models with just the
## intercept changing
mod <- lm(y ~ 1)
mod.mswm <- msmFit(mod, k = 2, sw =c(T, F))
##mod.mswm <- msmFit(mod, k = 2, p = 1, sw =c(T, F, F))

plotProb(mod.mswm, which = 1)
plotProb(mod.mswm, which = 2)

yhat <- rowSums(mod.mswm@Fit@CondMean * mod.mswm@Fit@smoProb[-1,])
plot(year, y, type = "o", col = "darkgrey", ylab = ylab, pch = 19, bty = "l")

##lines(year[-1], yhat)
lines(year, yhat)
