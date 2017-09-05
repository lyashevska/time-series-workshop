sim_gomp <- function(nu = 10, sigma_proc = 0.65,
  sigma_obs_true = 0.001, N = 50, b = 0.50,
  lambda = 1.5, y1 = 3, seed = NULL) {
  
  y <- vector(length = N)
  y[1] <- y1
  if (!is.null(seed)) set.seed(seed)
  proc_error <- metRology::rt.scaled(N, df = nu, mean = 0, sd = sigma_proc)
  # proc_error <- rnorm(N, mean = 0, sd = sigma_proc)
  for(i in 2:N) {
    y[i] <- lambda + b * y[i-1] + proc_error[i-1]
  }
  y <- rnorm(N, mean = y, sd = sigma_obs_true)
  y
}

plot(sim_gomp(b = 0.98, lambda = 1.5), type = "o")
plot(sim_gomp(b = 0, lambda = 1.5), type = "o")
plot(sim_gomp(b = 0.5, lambda = 1.5), type = "o")
plot(sim_gomp(b = -0.5, lambda = 1.5), type = "o")
plot(sim_gomp(b = 0.5, lambda = 0.1), type = "o")
plot(exp(sim_gomp(b = 0.5, lambda = 1)), type = "o")
plot(sim_gomp(b = 0.5, lambda = 1, seed = 42), type = "o")
lines(sim_gomp(b = 0.5, lambda = 1, seed = 42, sigma_obs_true = 0.3), type = "o", col = "red")
