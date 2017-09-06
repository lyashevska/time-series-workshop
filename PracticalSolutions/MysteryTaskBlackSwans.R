
#################################
##
#################################

library(rstan)
library(bayesplot)
library(ggplot2)
library(ggthemes)
library(cowplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(theme_excel())

## read in data
a <- read.csv('../data/a.csv')
b <- read.csv('../data/b.csv')
c <- read.csv('../data/c.csv')

## exploratory plots
par(mfrow = c(1,3))
plot(a, type = "b")
plot(b, type = "b")
plot(c, type = "b")

## 

data <- "c"

if(data == "a") {
y <- a$index
x <- a$year
}

if(data == "b") {
y <- b$index
x <- b$year
}

if(data == "c") {
y <- c$index
x <- c$year
}

## Fit gompertz
m <- stan("stan/gompertz.stan",
  data = list(N = length(y), y = log(y), nu_rate = 0.01),
  iter = 800)

# look at parameters
pars <- c("lambda", "b", "sigma_proc", "nu")
print(m, pars = pars)
traceplot(m, pars = pars)
mcmc_hist(as.matrix(m), pars = pars)

## Look at the posterior mean of nu
nu <- extract(m, pars = "nu")[[1]]
sum(nu < 10)/length(nu) # or mean(nu < 10)

## compare to the prior
prior <- rexp(1e6, rate = 0.01)
prior <- prior[prior > 2]
hist(prior)
median(prior)
abline(v = median(prior), col = "red", lwd = 2)

sum(prior < 10)/length(prior) # or mean(nu < 10)

## Plot the time series of predictions
e <- extract(m)
pred <- data.frame(
  est = apply(e$pred, 2, median),
  l = apply(e$pred, 2, quantile, probs = 0.025),
  h = apply(e$pred, 2, quantile, probs = 0.975),
  year = x)
pred$resid <- log(y) - pred$est
thresh <- qnorm(0.0001, 0, sd = median(e$sigma_proc))

outliers <- data.frame(outlier = rep(0, length(y)))
outliers$outlier[which(abs(pred$resid)>abs(thresh))] <- 1

pred$outlier <- outliers$outlier

p0 <- ggplot(data.frame(nu = nu), aes(nu)) + geom_histogram()

p1 <- ggplot(pred, aes(year, est, ymin = l, ymax = h)) +
  geom_ribbon(alpha = 0.4) +
  geom_line() +
  geom_point(aes(y = log(y), colour = as.factor(outlier))) +
  theme(legend.position = "none") + scale_colour_manual(values = c("black", "red"))

p2 <- ggplot(pred, aes(year, resid)) +
  geom_line() +
  geom_hline(yintercept = c(thresh, -thresh), lty = 2)


tog1 <- plot_grid(p0,p2, ncol = 2, align = 'h')
tog2 <- plot_grid(p1, ncol = 1, align = 'h')
tog3 <- plot_grid(tog1,tog2, nrow = 2)

if(data == "a") {
save_plot("Summary_a.png", tog3, ncol = 2, base_width = 5, base_height = 8)
}

if(data == "b") {
save_plot("Summary_b.png", tog3, ncol = 2, base_width = 5, base_height = 8)
}

if(data == "c") {
save_plot("Summary_c.png", tog3, ncol = 2, base_width = 5, base_height = 8)
}

