% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slise.R
\name{slise.explain}
\alias{slise.explain}
\title{SLISE for explaining Black box models.}
\usage{
slise.explain(
  X,
  Y,
  epsilon,
  x,
  y = NULL,
  lambda1 = 0,
  lambda2 = 0,
  weight = NULL,
  normalise = FALSE,
  logit = FALSE,
  initialisation = slise_initialisation_candidates,
  ...
)
}
\arguments{
\item{X}{Matrix of independent variables}

\item{Y}{Vector of the dependent variable}

\item{epsilon}{Error tolerance}

\item{x}{The sample to be explained (or index if y is null)}

\item{y}{The prediction to be explained (default: NULL)}

\item{lambda1}{L1 regularisation coefficient (default: 0)}

\item{lambda2}{L2 regularisation coefficient (default: 0)}

\item{weight}{Optional weight vector (default: NULL)}

\item{normalise}{Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)}

\item{logit}{Logit transform Y from probabilities to real values (default: FALSE)}

\item{initialisation}{function that gives the initial alpha and beta, or a list containing the initial alpha and beta (default: slise_initialisation_candidates)}

\item{...}{
  Arguments passed on to \code{\link[=graduated_optimisation]{graduated_optimisation}}, \code{\link[=slise_initialisation_candidates]{slise_initialisation_candidates}}
  \describe{
    \item{\code{beta_max}}{Stopping sigmoid steepness (default: 20 / epsilon^2)}
    \item{\code{max_approx}}{Approximation ratio when selecting the next beta (default: 1.15)}
    \item{\code{max_iterations}}{Maximum number of OWL-QN iterations (default: 300)}
    \item{\code{debug}}{Should debug statement be printed each iteration (default: FALSE)}
    \item{\code{num_init}}{the number of initial subsets to generate (default: 500)}
    \item{\code{beta_max_init}}{the maximum sigmoid steepness in the initialisation}
    \item{\code{pca_treshold}}{the maximum number of columns without using PCA (default: 10)}
  }}
}
\value{
slise.object
}
\description{
It is highly recommended that you normalise the data,
either before using SLISE or by setting normalise = TRUE.
}
\examples{
X <- matrix(rnorm(32), 8, 4)
Y <- runif(8, 0, 1)
expl <- slise.explain(X, Y, 0.1, 3, lambda1 = 0.01, logit = TRUE)
plot(expl, "bar", labels = c("class 1", "class 2"))
}
