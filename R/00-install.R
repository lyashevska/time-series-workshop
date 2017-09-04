install.packages(c("cpm", "MSwM", "dlm", "TMB", "loo", "rstan", 
  "ggplot2", "devtools", "bayesplot", "knitr", "rmarkdown"))
# devtools::install_github("fate-ewi/bayesdfa")
# devtools::install_github("seananderson/glmmfields", build_vignettes = TRUE)

# Install Rtools
# https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows

# Check that TMB is installed correctly:
TMB::runExample("simple")

# You may get some warnings, which you can safely ignore. 
# Check that the object `opt` exists. 

# Check that Stan is installed correctly:
stanmodelcode <- "
data {
int<lower=0> N;
real y[N];
} 

parameters {
real mu;
} 

model {
target += normal_lpdf(mu | 0, 10);
target += normal_lpdf(y  | mu, 1);
} 
"
y <- rnorm(20) 
dat <- list(N = 20, y = y); 
fit <- rstan::stan(model_code = stanmodelcode, model_name = "example", 
  data = dat, iter = 2012, chains = 1, sample_file = 'norm.csv',
  verbose = TRUE) 

# Again, you may get some C++ warnings that you can safely ignore.

# If you are having problems installing Stan see here:
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started