// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "slise_types.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// sigmoidc
arma::vec sigmoidc(const arma::vec& x);
RcppExport SEXP _slise_sigmoidc(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sigmoidc(x));
    return rcpp_result_gen;
END_RCPP
}
// loss_smooth_c
inline double loss_smooth_c(const arma::vec& alpha, const arma::mat& data, const arma::vec& response, const double& beta, const double& epsilon, const double& lambda);
RcppExport SEXP _slise_loss_smooth_c(SEXP alphaSEXP, SEXP dataSEXP, SEXP responseSEXP, SEXP betaSEXP, SEXP epsilonSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type response(responseSEXP);
    Rcpp::traits::input_parameter< const double& >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const double& >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< const double& >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(loss_smooth_c(alpha, data, response, beta, epsilon, lambda));
    return rcpp_result_gen;
END_RCPP
}
// loss_smooth_c_dc
Rcpp::NumericVector loss_smooth_c_dc(const SEXP xs, const SEXP dcptr);
RcppExport SEXP _slise_loss_smooth_c_dc(SEXP xsSEXP, SEXP dcptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type dcptr(dcptrSEXP);
    rcpp_result_gen = Rcpp::wrap(loss_smooth_c_dc(xs, dcptr));
    return rcpp_result_gen;
END_RCPP
}
// loss_smooth_grad_c
inline Rcpp::NumericVector loss_smooth_grad_c(const arma::vec& alpha, const arma::mat& data, const arma::vec& response, const double& beta, const double& epsilon, const double& lambda);
RcppExport SEXP _slise_loss_smooth_grad_c(SEXP alphaSEXP, SEXP dataSEXP, SEXP responseSEXP, SEXP betaSEXP, SEXP epsilonSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type response(responseSEXP);
    Rcpp::traits::input_parameter< const double& >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const double& >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< const double& >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(loss_smooth_grad_c(alpha, data, response, beta, epsilon, lambda));
    return rcpp_result_gen;
END_RCPP
}
// loss_smooth_grad_c_dc
Rcpp::NumericVector loss_smooth_grad_c_dc(const SEXP xs, const SEXP dcptr);
RcppExport SEXP _slise_loss_smooth_grad_c_dc(SEXP xsSEXP, SEXP dcptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type dcptr(dcptrSEXP);
    rcpp_result_gen = Rcpp::wrap(loss_smooth_grad_c_dc(xs, dcptr));
    return rcpp_result_gen;
END_RCPP
}
// lg_combined_smooth_c_dc
inline Rcpp::NumericVector lg_combined_smooth_c_dc(SEXP xs, SEXP dcptr);
RcppExport SEXP _slise_lg_combined_smooth_c_dc(SEXP xsSEXP, SEXP dcptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dcptr(dcptrSEXP);
    rcpp_result_gen = Rcpp::wrap(lg_combined_smooth_c_dc(xs, dcptr));
    return rcpp_result_gen;
END_RCPP
}
// lg_getgrad_c_dc
Rcpp::NumericVector lg_getgrad_c_dc(SEXP xs, SEXP dcptr);
RcppExport SEXP _slise_lg_getgrad_c_dc(SEXP xsSEXP, SEXP dcptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dcptr(dcptrSEXP);
    rcpp_result_gen = Rcpp::wrap(lg_getgrad_c_dc(xs, dcptr));
    return rcpp_result_gen;
END_RCPP
}
// loss_smooth_c_ptr
Rcpp::XPtr<funcPtr> loss_smooth_c_ptr();
RcppExport SEXP _slise_loss_smooth_c_ptr() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(loss_smooth_c_ptr());
    return rcpp_result_gen;
END_RCPP
}
// loss_smooth_grad_c_ptr
Rcpp::XPtr<funcPtr> loss_smooth_grad_c_ptr();
RcppExport SEXP _slise_loss_smooth_grad_c_ptr() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(loss_smooth_grad_c_ptr());
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_slise_sigmoidc", (DL_FUNC) &_slise_sigmoidc, 1},
    {"_slise_loss_smooth_c", (DL_FUNC) &_slise_loss_smooth_c, 6},
    {"_slise_loss_smooth_c_dc", (DL_FUNC) &_slise_loss_smooth_c_dc, 2},
    {"_slise_loss_smooth_grad_c", (DL_FUNC) &_slise_loss_smooth_grad_c, 6},
    {"_slise_loss_smooth_grad_c_dc", (DL_FUNC) &_slise_loss_smooth_grad_c_dc, 2},
    {"_slise_lg_combined_smooth_c_dc", (DL_FUNC) &_slise_lg_combined_smooth_c_dc, 2},
    {"_slise_lg_getgrad_c_dc", (DL_FUNC) &_slise_lg_getgrad_c_dc, 2},
    {"_slise_loss_smooth_c_ptr", (DL_FUNC) &_slise_loss_smooth_c_ptr, 0},
    {"_slise_loss_smooth_grad_c_ptr", (DL_FUNC) &_slise_loss_smooth_grad_c_ptr, 0},
    {"_rcpp_module_boot_mod", (DL_FUNC) &_rcpp_module_boot_mod, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_slise(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
