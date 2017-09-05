library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(42)
x <- runif(50)
y <- 0.3 + 0.8 * x + rnorm(50, 0, 0.2)
plot(x, y)
m <- stan("R/stan/lm.stan", data = list(x = x, y = y, N = length(x)))
m
lm(y ~ x)

plot(m)
traceplot(m)
shinystan::launch_shinystan(m)

m_priors <- stan("R/stan/lm-priors.stan", data = list(x = x, y = y, N = length(x)))
m_priors