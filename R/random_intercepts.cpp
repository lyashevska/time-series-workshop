#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() () {
  // DATA
  DATA_VECTOR(y);
  DATA_IVECTOR(gps);
  DATA_INTEGER(ngps);
  // PARAMETERS:
  PARAMETER(mu);
  PARAMETER(logsigma);
  PARAMETER(logtau);
  PARAMETER_VECTOR(u);
  // PRELIMINARY CALCULATIONS
  int n = y.size(); // number of observations
  Type sigma = exp(logsigma);
  Type tau = exp(logtau);
  vector<Type> gp_means(ngps);
  // PROCEDURE
  Type nll = 0.0; // initialize negative log likelihood
  // likelihood of the random effects
  for(int j = 0; j < ngps; j++){ 
    nll -= -log(tau) - log(2.0 * M_PI) / 2.0 - 
           pow(u(j), 2.0) / (2.0 * pow(tau, 2.0));
    gp_means(j) = mu + u(j);
  }
  // loop over the observations
  for(int i = 0; i < n; i++){ 
    nll -= -log(sigma) - log(2.0 * M_PI) / 2.0 - 
           pow(y(i) - (mu + u(gps(i))), 2.0) / (2.0 * pow(sigma, 2.0));
  }
  ADREPORT(gp_means);
  return nll;
}
