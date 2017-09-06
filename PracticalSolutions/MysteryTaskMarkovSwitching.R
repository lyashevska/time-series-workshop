## Mak
rm(list=ls())

# load data
a <- read.csv("../data/a.csv")
b <- read.csv("../data/b.csv")
c <- read.csv("../data/c.csv")

#####################
# plot data
a.year <- a$year
a.y <- a$index

plot( a.year, a.y, main="data a")

# markov switching
# intercept model
(a.mod <- lm(a.y ~ 1))
# slope model
#(a.mod <- lm(a.y ~ a.year))
## no AR on residuals
(a.mod.mswm <- msmFit(a.mod, k = 2, sw =c(T, F)))

plotProb(a.mod.mswm, which = 1)
plotProb(a.mod.mswm, which = 2)

## AR(1) turned on
(a.ar.mod.mswm <- msmFit(a.mod, k = 2, p = 1, sw =c(T, F, F)))
#(a.ar.mod.mswm <- msmFit(a.mod, k = 3, p = 1, sw =c(T, F, F, F)))

plotProb(a.ar.mod.mswm, which = 1)
plotProb(a.ar.mod.mswm, which = 2)


#################################
# plot data
b.year <- b$year
b.y <- b$index

plot( b.year, b.y, main="data b")

# markov switching
# intercept model
(b.mod <- lm(b.y ~ 1))
# slope model
#(b.mod <- lm(b.y ~ b.yebr))
## no AR on residuals
(b.mod.mswm <- msmFit(b.mod, k = 2, sw =c(T, F)))
plotProb(b.mod.mswm, which = 1)
plotProb(b.mod.mswm, which = 2)
## AR(1) turned on
(b.ar.mod.mswm <- msmFit(b.mod, k = 2, p = 1, sw =c(T, F, F)))
#(b.ar.mod.mswm <- msmFit(b.mod, k = 3, p = 1, sw =c(T, F, F)))

plotProb(b.ar.mod.mswm, which = 1)
plotProb(b.ar.mod.mswm, which = 2)
plotProb(b.ar.mod.mswm, which = 3)
#plotProb(b.ar.mod.mswm, which = 4)

###############################
# plot data
c.year <- c$year
c.y <- c$index

plot( c.year, c.y, main="data c", type="l")

# markov switching
# intercept model
(c.mod <- lm(c.y ~ 1))
# slope model
#(c.mod <- lm(c.y ~ c.year))
## no AR on residuals
(c.mod.mswm <- msmFit(c.mod, k = 2, sw =c(T, F)))
plotProb(c.mod.mswm, which = 1)
plotProb(c.mod.mswm, which = 2)
plotProb(c.mod.mswm, which = 3)

## AR(1) turned on
(c.ar.mod.mswm <- msmFit(c.mod, k = 2, p = 1, sw =c(T, F, F)))
#(c.ar.mod.mswm <- msmFit(c.mod, k = 3, p = 1, sw =c(T, F, F)))

plotProb(c.ar.mod.mswm, which = 1)
plotProb(c.ar.mod.mswm, which = 2)
plotProb(c.ar.mod.mswm, which = 3)
