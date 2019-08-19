
#include <RcppArmadillo.h>

typedef Rcpp::NumericVector (*funcPtr)(const SEXP, const SEXP);

arma::vec sigmoidc(const arma::vec&);
