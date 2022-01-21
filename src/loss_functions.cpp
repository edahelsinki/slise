/*
   Loss and gradient functions in Rcpp / RcppArmadillo.

   Usage in R:
      library(Rcpp)
      sourceCpp("loss_functions.cpp")

   Classes:
       (1) DataContainer
           - Contains the following fields:
	      * data (matrix)
	      * response (vector)
	      * beta, epsilon, lambda1, lambda2 (doubles)

	  - Each field has a getter / setter: getData(), setData() etc.

       - To use this in R:
          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), epsilon = 0.4, beta = 1, lambda1 = 0.3, lambda2 = 0.0)
	  dc$getBeta()  ## returns 1
	  dc$setBeta(5) ## set the value of beta to 5
	  dc$getBeta()  ## returns 5


   Functions:
      (1) loss_smooth_c(vec alpha, mat data, vec response, double epsilon, double beta, double lambda1, double lambda2)
          - This function returns the value of the loss function (double).
          - Call from R by giving the appropriate parameters.

      (2) loss_smooth_grad_c(vec alpha, mat data, vec response, double epsilon, double beta, double lambda1, double lambda2)
          - This function returns the gradient of the loss function (vector).
          - Call from R by giving the appropriate parameters.


      (3) loss_smooth_c_dc(vec alpha, DataContainer dc)
          - This function returns the value of the loss function (double).
          - This is a wrapper for loss_smooth_c.
          - To use this function, first create a DataContainer in R:

          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), epsilon = 0.4, beta = 1, lambda1 = 0.3, lambda2 = 0.0)
	  loss_smooth_c_dc(xs = runif(5), dcptr = dc$.pointer)

      (4) loss_smooth_grad_c_dc(vec alpha, DataContainer dc)
          - This function returns the gradient of the loss function (vector).
          - This is a wrapper for loss_smooth_grad_c.
          - To use this function, first create a DataContainer in R:

          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), epsilon = 0.4, beta = 1, lambda1 = 0.3, lambda2 = 0.0)
	  loss_smooth_c_dc(xs = runif(5), dcptr = dc$.pointer)


      (5) lg_combined_smooth_c_dc(vec alpha, DataContainer dc)
          - This function calculates both the loss and the gradient at the same time
          - The loss (double) is returned and both the loss (double) and gradient (vector) are stored in the DataContainer.
	  - To use this function, first create a DataContainer in R:

          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), epsilon = 0.4, beta = 1, lambda1 = 0.3, lambda2 = 0.0)
	  lg_combined_smooth_c_dc(xs = runif(5), dcptr = dc$.pointer) ## returns the loss

	  dc$getLoss() # get the loss value
	  dc$getGrad() # get the gradient

      (6) lg_getgrad_c_dc(vec alpha, DataContainer dc)
          - This function only returns the gradient found in dc (similar to dc$getGrad())
          - This is to be used with LBFGS.


      Usage with LBFGS:
           dc    <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), epsilon = 0.4, beta = 1, lambda1 = 0.0, lambda2 = 0.0)
           alpha <- runif(5)
           lbfgs(loss_smooth_c_ptr(), loss_smooth_grad_c_ptr(), alpha, dc$.pointer, max_iterations = 100, invisible = TRUE, orthantwise_c = lambda1),
 */

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

/* --------------------------------------------------
  Class for holding the data
  -------------------------------------------------- */

class DataContainer;

class DataContainer
{
public:
    arma::mat data;
    arma::vec response;
    double epsilon, beta, lambda1, lambda2;
    arma::vec weight;
    double ws; // Should always be updated to arma::accu(weight) (== sum(weight))

    double loss;
    arma::vec grad;

    DataContainer(arma::mat data_, arma::vec response_, double epsilon_, double beta_, double lambda1_, double lambda2_, arma::vec weight_) : data(data_), response(response_), epsilon(epsilon_), beta(beta_), lambda1(lambda1_), lambda2(lambda2_), weight(weight_), ws(arma::accu(weight_)){};
    DataContainer(Rcpp::NumericMatrix data_, Rcpp::NumericVector response_, double epsilon_, double beta_, double lambda1_, double lambda2_, Rcpp::NumericVector weight_) : DataContainer(Rcpp::as<arma::mat>(data_), Rcpp::as<arma::colvec>(response_), epsilon_, beta_, lambda1_, lambda2_, Rcpp::as<arma::colvec>(weight_)){};
    DataContainer(Rcpp::NumericMatrix data_, Rcpp::NumericVector response_, double epsilon_, double beta_, double lambda1_, double lambda2_) : DataContainer(Rcpp::as<arma::mat>(data_), Rcpp::as<arma::colvec>(response_), epsilon_, beta_, lambda1_, lambda2_, arma::zeros(0)){};
    DataContainer(Rcpp::NumericMatrix data_, Rcpp::NumericVector response_, double epsilon_, double beta_, Rcpp::NumericVector weight_) : DataContainer(Rcpp::as<arma::mat>(data_), Rcpp::as<arma::colvec>(response_), epsilon_, beta_, 0.0, 0.0, Rcpp::as<arma::colvec>(weight_)){};
    DataContainer(Rcpp::NumericMatrix data_, Rcpp::NumericVector response_, double epsilon_, double beta_) : DataContainer(Rcpp::as<arma::mat>(data_), Rcpp::as<arma::colvec>(response_), epsilon_, beta_, 0.0, 0.0, arma::zeros(0)){};
    DataContainer(const DataContainer &i) : data(i.data), response(i.response), epsilon(i.epsilon), beta(i.beta), lambda1(i.lambda1), lambda2(i.lambda2), weight(i.weight), ws(i.ws) {}

    Rcpp::NumericMatrix getData() { return Rcpp::wrap(data); }
    Rcpp::NumericVector getResponse() { return Rcpp::wrap(response); }
    double getEpsilon() { return epsilon; }
    double getBeta() { return beta; }
    double getLambda1() { return lambda1; }
    double getLambda2() { return lambda2; }
    double getLoss() { return loss; }
    Rcpp::NumericVector getWeight() { return Rcpp::wrap(weight); }
    Rcpp::NumericVector getGrad() { return Rcpp::wrap(grad); }

    void setData(Rcpp::NumericMatrix M) { data = Rcpp::as<arma::mat>(M); }
    void setResponse(Rcpp::NumericVector v) { response = Rcpp::as<arma::colvec>(v); }
    void setEpsilon(double x) { epsilon = x; }
    void setBeta(double x) { beta = x; }
    void setLambda1(double x) { lambda1 = x; }
    void setLambda2(double x) { lambda2 = x; }
    void setWeight(Rcpp::NumericVector v)
    {
        weight = Rcpp::as<arma::colvec>(v);
        ws = arma::accu(weight);
    }
    void setGrad(Rcpp::NumericVector v) { grad = Rcpp::as<arma::colvec>(v); }
};

RCPP_EXPOSED_CLASS(DataContainer)

RCPP_MODULE(slise_mod)
{
    Rcpp::class_<DataContainer>("DataContainer")
        .constructor<Rcpp::NumericMatrix, Rcpp::NumericVector, double, double, double, double, Rcpp::NumericVector>()
        .constructor<Rcpp::NumericMatrix, Rcpp::NumericVector, double, double, double, double>()
        .constructor<Rcpp::NumericMatrix, Rcpp::NumericVector, double, double, Rcpp::NumericVector>()
        .constructor<Rcpp::NumericMatrix, Rcpp::NumericVector, double, double>()
        .constructor<DataContainer>()

        .method("getData", &DataContainer::getData)
        .method("getResponse", &DataContainer::getResponse)
        .method("getBeta", &DataContainer::getBeta)
        .method("getEpsilon", &DataContainer::getEpsilon)
        .method("getLambda1", &DataContainer::getLambda1)
        .method("getLambda2", &DataContainer::getLambda2)
        .method("getWeight", &DataContainer::getWeight)
        .method("getLoss", &DataContainer::getLoss)
        .method("getGrad", &DataContainer::getGrad)

        .method("setData", &DataContainer::setData)
        .method("setResponse", &DataContainer::setResponse)
        .method("setBeta", &DataContainer::setBeta)
        .method("setEpsilon", &DataContainer::setEpsilon)
        .method("setLambda1", &DataContainer::setLambda1)
        .method("setLambda2", &DataContainer::setLambda2)
        .method("setWeight", &DataContainer::setWeight)
        .method("setGrad", &DataContainer::setGrad)

        ;
}

/* --------------------------------------------------
   Utility functions.
  -------------------------------------------------- */

// [[Rcpp::export]]
arma::vec sigmoidc(const arma::vec &x)
{
    return (1 / (1 + arma::exp(-x)));
}

inline arma::vec i_sigmoidc(const arma::vec &x)
{
    return (1 / (1 + arma::exp(-x)));
}

// [[Rcpp::export]]
arma::vec log_sigmoidc(const arma::vec &x)
{
    arma::vec res(x.size());
    for (arma::uword i = 0; i < x.size(); i++)
    {
        const double val = x[i];
        res[i] = val >= 0 ? -log(1 + exp(-val)) : val - log(1 + exp(val));
    }
    return res;
}

inline arma::vec pmin(const arma::vec &a, const double b)
{
    arma::vec res(a.size());
    for (arma::uword i = 0; i < a.size(); i++)
    {
        const double val = a[i];
        res[i] = val < b ? val : b;
    }
    return res;
}

inline arma::vec pmin_other(const arma::vec &a, const arma::vec &b, const double c)
{
    arma::vec res(a.size());
    for (arma::uword i = 0; i < a.size(); i++)
        res[i] = b[i] < c ? a[i] : c;
    return res;
}

/* --------------------------------------------------
   Smooth loss function.

   alpha	: alpha vector
   data		: data matrix
   response	: response vector
   epsilon	: epsilon (double)
   beta		: beta (double)
   lambda1	: lambda1 (double)
   lambda2	: lambda2 (double)
   weight   : weight vector
  -------------------------------------------------- */

// [[Rcpp::export]]
double loss_smooth_c(const arma::vec &alpha,
                     const arma::mat &data,
                     const arma::vec &response,
                     const double &epsilon,
                     const double &beta,
                     const double &lambda1,
                     const double &lambda2,
                     const arma::vec &weight)
{

    // calculate loss
    double epsilon2 = pow(epsilon, 2);
    arma::vec distances = arma::pow(data * alpha - response, 2);
    arma::vec subset = i_sigmoidc(beta * (epsilon2 - distances));

    double loss;
    if (weight.size() > 0)
    {
        double length = arma::accu(weight);
        arma::vec residuals = pmin(distances - (epsilon2 * length), 0);
        loss = arma::accu(subset % residuals % weight) / length;
    }
    else
    {
        arma::vec residuals = pmin(distances - epsilon2 * response.size(), 0);
        loss = arma::accu(subset % residuals) / distances.size();
    }

    if (lambda1 > 0)
        loss += (lambda1 * arma::accu(arma::abs(alpha)));
    if (lambda2 > 0)
        loss += (lambda2 * arma::accu(alpha % alpha));

    return loss;
}

/* --------------------------------------------------
   Smooth loss function using a DataContainer.
   The DataContainer contains the parameters
   (data, response, beta, epsilon, lambda1, lambda2)
   which are then used to call loss_smooth_c().

   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector loss_smooth_c_dc(const SEXP xs, const SEXP dcptr)
{
    const Rcpp::XPtr<DataContainer> dc(dcptr);

    Rcpp::NumericVector out(1);
    out[0] = loss_smooth_c(Rcpp::as<arma::vec>(xs), dc->data, dc->response, dc->epsilon, dc->beta, dc->lambda1, dc->lambda2, dc->weight);

    return out;
}

/* --------------------------------------------------
   Gradient of the smooth loss function.

   alpha	: alpha vector
   data		: data matrix
   response	: response vector
   epsilon	: epsilon (double)
   beta		: beta (double)
   lambda1	: lambda1 (double)
   lambda2	: lambda2 (double)
   weight   : weight vector
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector loss_smooth_grad_c(const arma::vec &alpha,
                                       const arma::mat &data,
                                       const arma::vec &response,
                                       const double &epsilon,
                                       const double &beta,
                                       const double &lambda1,
                                       const double &lambda2,
                                       const arma::vec &weight)
{

    double epsilon2 = pow(epsilon, 2);
    double length;
    if (weight.size() > 0)
        length = arma::accu(weight);
    else
        length = data.n_rows;
    arma::vec distances = data * alpha - response;
    arma::colvec distances2 = arma::pow(distances, 2);

    arma::colvec f = distances2 - epsilon2 * length;
    arma::colvec s = i_sigmoidc(beta * (epsilon2 - distances2));
    double k1 = 2.0 / length;
    arma::colvec k2 = (-2.0 * beta / length) * (s - pow(s, 2));
    distances = pmin_other(distances, f, 0); //phi(x) ~ pmin(x, 0)

    arma::vec grad;
    if (weight.size() > 0)
        grad = (data.each_col() % (distances % weight)).t() * ((s * k1) + (f % k2));
    else
        grad = (data.each_col() % distances).t() * ((s * k1) + (f % k2));

    if (lambda1 > 0)
        grad += (lambda1 * arma::sign(alpha));
    if (lambda2 > 0)
        grad += ((lambda2 * 2) * alpha);

    return Rcpp::wrap(grad);
}

/* --------------------------------------------------
   Gradient of smooth loss function using a DataContainer.
   The DataContainer contains the parameters
   data, response, beta, epsilon, lambda1, lambda2)
   which are then used to call loss_smooth_c().

   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector loss_smooth_grad_c_dc(const SEXP xs, const SEXP dcptr)
{
    const Rcpp::XPtr<DataContainer> dc(dcptr);
    return loss_smooth_grad_c(Rcpp::as<arma::vec>(xs), dc->data, dc->response, dc->epsilon, dc->beta, dc->lambda1, dc->lambda2, dc->weight);
}

/* --------------------------------------------------
   Calculate loss and gradient simultaneously,
   using a data container.

   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector lg_combined_smooth_c_dc(SEXP xs, SEXP dcptr)
{
    const arma::vec alpha = Rcpp::as<arma::vec>(xs);
    const Rcpp::XPtr<DataContainer> dc(dcptr);

    // calculate loss
    double epsilon2 = pow(dc->epsilon, 2);
    double length;
    if (dc->weight.size() > 0)
        length = dc->ws;
    else
        length = dc->data.n_rows;

    arma::colvec distances = dc->data * alpha - dc->response;
    arma::colvec distances2 = arma::pow(distances, 2);

    arma::colvec subset = i_sigmoidc(dc->beta * (epsilon2 - distances2));
    arma::colvec residuals = distances2 - epsilon2 * length;

    if (dc->weight.size() > 0)
        dc->loss = arma::accu(subset % pmin(residuals, 0) % dc->weight) / length;
    else
        dc->loss = arma::accu(subset % pmin(residuals, 0)) / length;

    // calculate gradient
    double k1 = 2.0 / length;
    arma::colvec k2 = (-2.0 * dc->beta / length) * (subset - pow(subset, 2));
    distances = pmin_other(distances, residuals, 0);

    if (dc->weight.size() > 0)
        dc->grad = (dc->data.each_col() % (distances % dc->weight)).t() * ((subset * k1) + (residuals % k2));
    else
        dc->grad = (dc->data.each_col() % distances).t() * ((subset * k1) + (residuals % k2));

    // check lambda
    if (dc->lambda1 > 0)
    {
        dc->loss += (dc->lambda1 * arma::accu(arma::abs(alpha)));
        dc->grad += (dc->lambda1 * arma::sign(alpha));
    }
    if (dc->lambda2 > 0)
    {
        dc->loss += (dc->lambda2 * arma::accu(alpha % alpha));
        dc->grad += ((dc->lambda2 * 2) * alpha);
    }

    Rcpp::NumericVector out(1);
    out[0] = dc->loss;
    return out;
}

/* --------------------------------------------------
   Return gradient.
   This assumes that the gradient has first been
   calculated and stored in the DataContainer.

   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector lg_getgrad_c_dc(SEXP xs, SEXP dcptr)
{
    const Rcpp::XPtr<DataContainer> dc(dcptr);
    return Rcpp::wrap(dc->grad);
}

/* --------------------------------------------------
   Return function pointers to the loss and gradient
   functions for use with lbfgs.
  -------------------------------------------------- */

typedef Rcpp::NumericVector (*funcPtr)(const SEXP, const SEXP);

// [[Rcpp::export]]
Rcpp::XPtr<funcPtr> loss_smooth_c_ptr()
{
    return (Rcpp::XPtr<funcPtr>(new funcPtr(&lg_combined_smooth_c_dc)));
}

// [[Rcpp::export]]
Rcpp::XPtr<funcPtr> loss_smooth_grad_c_ptr()
{
    return (Rcpp::XPtr<funcPtr>(new funcPtr(&lg_getgrad_c_dc)));
}
