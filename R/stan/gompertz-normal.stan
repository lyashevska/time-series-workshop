data {
  int<lower=0> N;        // rows of data
  vector[N] y;           // vector to hold observations
}
parameters {
  real lambda;
  real b;
  real<lower=0> sigma_proc;
}
model {
  // priors
  lambda ~ normal(0, 5);
  b ~ normal(0, 5);
  sigma_proc ~ student_t(3, 0, 3);
  
  // likelihood
  for (i in 2:N) {
    y[i] ~ normal(lambda + b * y[i-1], sigma_proc);
  }
}
generated quantities {
  vector[N-1] log_lik; // log_lik is for use with the loo package
  vector[N] pred;
  pred[1] = y[1];
  for (i in 2:N) {
    pred[i] = normal_rng(lambda + b * y[i-1], sigma_proc);
  }
  for (i in 2:N) {
    log_lik[i-1] = normal_lpdf(y[i] | y[i-1], sigma_proc);
  }
}
