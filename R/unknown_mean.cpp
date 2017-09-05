#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() () {
  // DATA
  DATA_VECTOR(y);
  // PARAMETERS:
  PARAMETER(mu); 
  // PRELIMINARY CALCULATIONS
  int n = y.size(); // number of observations
  // PROCEDURE
  Type nll = 0.0; // initialize negative log likelihood
  // loop over the observations
  for(int i = 0; i < n; i++){ 
    nll -= -log(2.0) - log(2.0 * M_PI) / 2.0 - pow(y(i) - mu, 2.0) / 8.0;
  }
  return nll;
}
