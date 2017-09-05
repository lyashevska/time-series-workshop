#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() () {
  // DATA
  DATA_VECTOR(y);
  // PARAMETERS:
  PARAMETER(logsdproc);
  PARAMETER(logsdmeas);
  PARAMETER_VECTOR(alpha);
  // PRELIMINARY CALCULATIONS
  int n = y.size(); // number of observations
  Type sdproc = exp(logsdproc);
  Type sdmeas = exp(logsdmeas);
  // PROCEDURE
  Type nll = 0.0; // initialize negative log likelihood
  // likelihood of the random effects
  for(int i = 1; i < n; i++){
    nll -= log(0.95  * dnorm(alpha(i), alpha(i - 1), sdproc) +
	       0.05  * dnorm(alpha(i), alpha(i - 1), (10.0 * sdproc)));
    //nll -= dnorm(alpha(i), alpha(i - 1), sdproc, true);
  }
  // cout can be very useful!
  // std::cout << "Made it here?" << std::endl  << std::endl;
  // loop over the observations
  for(int i = 0; i < n; i++){ 
    nll -= dnorm(y(i), alpha(i), sdmeas, true);
  }
  vector<Type> pwide(n);
  for(int i = 1; i < n; i++){ 
    pwide(i)= 0.95 * dnorm(alpha(i), alpha(i-1), sdproc) / 
      (0.95 * dnorm(alpha(i), alpha(i-1), sdproc) + 
       0.05 * dnorm(alpha(i), alpha(i-1), 10 * sdproc));
  }
  ADREPORT(pwide);
  return nll;
}
