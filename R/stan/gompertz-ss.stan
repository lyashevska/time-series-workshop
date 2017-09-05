data {
  int<lower=0> N;        // rows of data
  vector[N] y;           // vector to hold observations
  real<lower=0> nu_rate; // rate parameter for nu exponential prior
  real<lower=0> sigma_obs; // observation SD on log scale
}
parameters {
  real lambda;
  real b;
  real<lower=0> sigma_proc;
  real<lower=2> nu;
  vector[N] U; // states
}
model {
  // priors
  lambda ~ normal(0, 5);
  b ~ normal(0, 5);
  sigma_proc ~ student_t(3, 0, 3);
  nu ~ exponential(nu_rate);
  
  for (i in 2:N) {
    U[i] ~ student_t(nu, lambda + b * U[i-1], sigma_proc);
  }
  y ~ normal(U, sigma_obs);
}
generated quantities {
  vector[N] pred;
  pred[1] = U[1];
  for (i in 2:N) {
    pred[i] = student_t_rng(nu, lambda + b * U[i-1], sigma_proc);
  }
}
