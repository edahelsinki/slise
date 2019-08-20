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
	      * beta, epsilon, lambda (doubles)

	  - Each field has a getter / setter: getData(), setData() etc.

       - To use this in R:
          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), beta = 1, epsilon = 0.4, lambda = 0.3)
	  dc$getBeta()  ## returns 1
	  dc$setBeta(5) ## set the value of beta to 5
	  dc$getBeta()  ## returns 5


   Functions:
      (1) loss_smooth_c(vec alpha, mat data, vec response, double beta, double epsilon, double lambda)
          - This function returns the value of the loss function (double).
          - Call from R by giving the appropriate parameters.

      (2) loss_smooth_grad_c(vec alpha, mat data, vec response, double beta, double epsilon, double lambda)
          - This function returns the gradient of the loss function (vector).
          - Call from R by giving the appropriate parameters.


      (3) loss_smooth_c_dc(vec alpha, DataContainer dc)
          - This function returns the value of the loss function (double).
          - This is a wrapper for loss_smooth_c.
          - To use this function, first create a DataContainer in R:
          
          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), beta = 1, epsilon = 0.4, lambda = 0.3)
	  loss_smooth_c_dc(xs = runif(5), dcptr = dc$.pointer)

      (4) loss_smooth_grad_c_dc(vec alpha, DataContainer dc)
          - This function returns the gradient of the loss function (vector).
          - This is a wrapper for loss_smooth_grad_c.
          - To use this function, first create a DataContainer in R:
          
          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), beta = 1, epsilon = 0.4, lambda = 0.3)
	  loss_smooth_c_dc(xs = runif(5), dcptr = dc$.pointer)


      (5) lg_combined_smooth_c_dc(vec alpha, DataContainer dc)
          - This function calculates both the loss and the gradient at the same time
          - The loss (double) is returned and both the loss (double) and gradient (vector) are stored in the DataContainer.
	  - To use this function, first create a DataContainer in R:

          dc <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), beta = 1, epsilon = 0.4, lambda = 0.3)
	  lg_combined_smooth_c_dc(xs = runif(5), dcptr = dc$.pointer) ## returns the loss
	  
	  dc$getLoss() # get the loss value
	  dc$getGrad() # get the gradient

      (6) lg_getgrad_c_dc(vec alpha, DataContainer dc)
          - This function only returns the gradient found in dc (similar to dc$getGrad())
          - This is to be used with LBFGS.


      Usage with LBFGS:
           dc    <- new(DataContainer, data = matrix(runif(50), nrow = 10), response = runif(10), beta = 1, epsilon = 0.4, lambda = 0.3)
           alpha <- runif(5)
           lbfgs(loss_smooth_c_ptr(), loss_smooth_grad_c_ptr(), alpha, dc$.pointer, max_iterations = 100, invisible = TRUE, orthantwise_c = dc$getLambda()),
 */



// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


/* --------------------------------------------------
  Class for holding the data 
  -------------------------------------------------- */

class DataContainer;

//_' @export DataContainer
class DataContainer {
public:
    arma::mat data;
    arma::vec response;
    double beta, epsilon, lambda;

    double loss;
    arma::vec grad;

    DataContainer(Rcpp::NumericMatrix data_, Rcpp::NumericVector response_, double beta_, double epsilon_, double lambda_) : data ( Rcpp::as<arma::mat>(data_) ), response ( Rcpp::as<arma::colvec>(response_) ), beta( beta_ ), epsilon( epsilon_ ), lambda( lambda_ ) {};
    DataContainer(Rcpp::NumericMatrix data_, Rcpp::NumericVector response_, double beta_, double epsilon_) : DataContainer(data_, response_, beta_, epsilon_, 0) {};
    DataContainer(const DataContainer & i) : data( i.data ), response( i.response), beta( i.beta), epsilon( i.epsilon ), lambda( i.lambda) {}

    Rcpp::NumericMatrix getData()     { return Rcpp::wrap(data); }
    Rcpp::NumericVector getResponse() { return Rcpp::wrap(response); }
    double getBeta()                  { return beta; }
    double getEpsilon()               { return epsilon; }
    double getLambda ()               { return lambda; }
    double getLoss()                  { return loss; }
    Rcpp::NumericVector getGrad()     { return Rcpp::wrap(grad); }
    
    void setData(Rcpp::NumericMatrix M)     { data = Rcpp::as<arma::mat>(M); } 
    void setResponse(Rcpp::NumericVector v) { response = Rcpp::as<arma::colvec>(v); }
    void setBeta(double x)            { beta = x; }
    void setEpsilon(double x)         { epsilon = x; }
    void setLambda(double x)          { lambda = x; }
    void setGrad(Rcpp::NumericVector v)     { grad = Rcpp::as<arma::colvec>(v); }

};

RCPP_EXPOSED_CLASS(DataContainer)

RCPP_MODULE(slise_mod) {
    Rcpp::class_<DataContainer>("DataContainer")
        .constructor< Rcpp::NumericMatrix, Rcpp::NumericVector, double, double, double >()
        .constructor< Rcpp::NumericMatrix, Rcpp::NumericVector, double, double >()
        .constructor< DataContainer >()
        .method("getData", &DataContainer::getData)
	.method("getResponse", &DataContainer::getResponse)
	.method("getBeta", &DataContainer::getBeta)
	.method("getEpsilon", &DataContainer::getEpsilon)
	.method("getLambda", &DataContainer::getLambda)
	.method("getLoss", &DataContainer::getLoss)
	.method("getGrad", &DataContainer::getGrad)
	
	
	.method("setData", &DataContainer::setData)
	.method("setResponse", &DataContainer::setResponse)
	.method("setBeta", &DataContainer::setBeta)
	.method("setEpsilon", &DataContainer::setEpsilon)
	.method("setLambda", &DataContainer::setLambda)
	.method("setGrad", &DataContainer::setGrad)

	;
}


/* --------------------------------------------------
   Sigmoid function written in c++.
  -------------------------------------------------- */
// [[Rcpp::export]]
arma::vec sigmoidc(const arma::vec& x) {
    return (1 / (1 + arma::exp(-x)));
    // return (arma::exp(x) / (arma::exp(x) + 1));
}
inline arma::vec i_sigmoidc(const arma::vec& x) {
    return (1 / (1 + arma::exp(-x)));
}

/* --------------------------------------------------
   Dot product.
  -------------------------------------------------- */
double dotprod(const arma::vec& a, const arma::vec& b) {
    double res = 0;
    for (int i = 0; i < a.size(); i++)
	res += a[i] * b[i];
    return res;
}

/* --------------------------------------------------
   Clamp max.
  -------------------------------------------------- */
inline arma::vec clamp_max(const arma::vec& a, double b) {
    arma::vec res(a.size());
    for(int i = 0; i < a.size(); i++)
        res[i] = a[i] < b ? a[i] : b;
    return res;
}

/* --------------------------------------------------
   Clamp min.
  -------------------------------------------------- */
inline arma::vec clamp_min(const arma::vec& a, double b) {
    arma::vec res(a.size());
    for(int i = 0; i < a.size(); i++)
        res[i] = a[i] > b ? a[i] : b;
    return res;
}


/* --------------------------------------------------
   Clamp min according to another vector.
  -------------------------------------------------- */
inline arma::vec clamp_max_other(const arma::vec& a, const arma::vec& b, double c) {
    arma::vec res(a.size());
    for(int i = 0; i < a.size(); i++)
        res[i] = b[i] < c ? a[i] : c;
    return res;
}


/* --------------------------------------------------
   Smooth loss function.

   alpha	: alpha vector
   data		: data matrix
   response	: response vector
   beta		: beta (double)
   epsilon	: epsilon (double)
   lambda	: lambda (double)
  -------------------------------------------------- */

// [[Rcpp::export]]
inline double loss_smooth_c(const arma::vec& alpha,
			    const arma::mat& data,
			    const arma::vec& response,
			    const double& beta,
			    const double& epsilon,
			    const double& lambda = 0) {
    
    // calculate loss
    double epsilonx = pow(epsilon, 2);
    double epsilony = epsilonx * response.size();
    double betax = beta / epsilonx;
    
    arma::vec distances = arma::pow(data * alpha - response, 2);
    arma::vec subsize   = i_sigmoidc(betax * (epsilonx - distances));
    arma::vec loss      = clamp_max(distances - epsilony, 0); //phi(x) ~ clamp_max(x, 0)

    double out = arma::accu(subsize % loss) / distances.size();

    if (lambda > 0)
        out += (lambda * arma::accu(arma::abs(alpha)));

    return out;
}


/* --------------------------------------------------
   Smooth loss function using a DataContainer.
   The DataContainer contains the parameters
   (data, response, beta, epsilon, lambda) which
   are then used to call loss_smooth_c().
   
   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector loss_smooth_c_dc(const SEXP xs, const SEXP dcptr) {
    const Rcpp::XPtr<DataContainer> dc(dcptr);

    Rcpp::NumericVector out(1);
    out[0] = loss_smooth_c(Rcpp::as<arma::vec>(xs), dc->data, dc->response, dc->beta, dc->epsilon, dc->lambda);
    
    return out;
}


/* --------------------------------------------------
   Gradient of the smooth loss function.

   alpha	: alpha vector
   data		: data matrix
   response	: response vector
   beta		: beta (double)
   epsilon	: epsilon (double)
   lambda	: lambda (double)
  -------------------------------------------------- */

// [[Rcpp::export]]
inline Rcpp::NumericVector loss_smooth_grad_c(const arma::vec& alpha,
					      const arma::mat& data,
					      const arma::vec& response,
					      const double& beta,
					      const double& epsilon,
					      const double& lambda = 0) {

    double epsilonx = pow(epsilon, 2);
    double betax = beta / epsilonx;

    arma::vec distances  = data * alpha - response;
    arma::colvec distances2 = arma::pow(distances, 2);

    arma::colvec f = distances2 / data.n_rows - epsilonx;
    arma::colvec s = i_sigmoidc(betax * (epsilonx - distances2));

    double k1 = 2.0 / data.n_rows;
    arma::colvec k2 = -2.0 * betax * (s - pow(s, 2));

    distances = clamp_max_other(distances, f, 0); //phi(x) ~ clamp_max(x, 0)

    arma::vec out = (data.each_col() % distances).t() * ((s * k1) + (f % k2));
    
    if (lambda > 0)
        out += (lambda * arma::sign(alpha));

    return Rcpp::wrap(out);
}


/* --------------------------------------------------

   Gradient of smooth loss function using a DataContainer.
   The DataContainer contains the parameters
   data, response, beta, epsilon, lambda) which are
   then used to call loss_smooth_c().
   
   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
Rcpp::NumericVector loss_smooth_grad_c_dc(const SEXP xs, const SEXP dcptr) {
    const Rcpp::XPtr<DataContainer> dc(dcptr);

    return loss_smooth_grad_c(Rcpp::as<arma::vec>(xs), dc->data, dc->response, dc->beta, dc->epsilon, dc->lambda);

}


/* --------------------------------------------------
   Calculate loss and gradient simultaneously,
   using a data container.
   
   xs    : alpha vector
   dcptr : pointer to a DataContainer (from R)
  -------------------------------------------------- */

// [[Rcpp::export]]
inline Rcpp::NumericVector lg_combined_smooth_c_dc(SEXP xs, SEXP dcptr) {
    const arma::vec alpha = Rcpp::as<arma::vec>(xs);
    const Rcpp::XPtr<DataContainer> dc(dcptr);

    // calculate loss
    double epsilonx = pow(dc->epsilon, 2);
    double betax = dc->beta / epsilonx;
    
    arma::colvec distances  = dc->data * alpha - dc->response;
    arma::colvec distances2 = arma::pow(distances, 2);
    
    arma::colvec subsize   = i_sigmoidc(betax * (epsilonx - distances2));
    arma::colvec loss = distances2 / dc->data.n_rows - epsilonx;

    dc->loss = arma::accu(subsize % clamp_max(loss, 0));  //phi(x) ~ clamp_max(x, 0)
    
    // calculate gradient
    double k1 = 2.0 / dc->data.n_rows;
    arma::colvec k2 = -2.0 * betax * (subsize - pow(subsize, 2));

    distances = clamp_max_other(distances, loss, 0); //phi(x) ~ clamp_max(x, 0)
    dc->grad = (dc->data.each_col() % distances).t() * ((subsize * k1) + (loss % k2));
	
    // check lambda
    if (dc->lambda > 0) {
        dc->loss += (dc->lambda * arma::accu(arma::abs(alpha)));
        dc->grad += (dc->lambda * arma::sign(alpha));
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
Rcpp::NumericVector lg_getgrad_c_dc(SEXP xs, SEXP dcptr) {
    const Rcpp::XPtr<DataContainer> dc(dcptr);
    return Rcpp::wrap(dc->grad);
}



/* --------------------------------------------------
   Return function pointers to the loss and gradient
   functions for use with lbfgs.
  -------------------------------------------------- */

typedef Rcpp::NumericVector (*funcPtr)(const SEXP, const SEXP);

// [[Rcpp::export]]
Rcpp::XPtr<funcPtr> loss_smooth_c_ptr() {
    // return(XPtr<funcPtr>(new funcPtr(&loss_smooth_c_dc)));
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&lg_combined_smooth_c_dc)));
}


// [[Rcpp::export]]
Rcpp::XPtr<funcPtr> loss_smooth_grad_c_ptr() {
    // return(XPtr<funcPtr>(new funcPtr(&loss_smooth_grad_c_dc)));
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&lg_getgrad_c_dc)));
}
