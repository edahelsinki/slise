% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{slise.explain}
\alias{slise.explain}
\title{SLISE Black Box Explainer
Use SLISE for explaining predictions made by a black box.}
\usage{
slise.explain(X, Y, x, y = NULL, epsilon = 0.1, lambda = 0, ...,
  scale = FALSE, logit = FALSE, scale_y = TRUE)
}
\arguments{
\item{X}{matrix of independent variables}

\item{Y}{vector of the dependent variable}

\item{x}{the sample to be explained (or index if y is null)}

\item{y}{the prediction to be explained}

\item{epsilon}{error tolerance (will be scaled to represent a percentage, e.g. 0.1 == 10\%)}

\item{lambda}{sparsity reguraliser}

\item{...}{other parameters to the optimiser}

\item{scale}{Scale X by mean and standard deviation (FALSE)}

\item{logit}{Should Y be logit-transformed (recommended for probabilities) (FALSE)}

\item{scale_y}{Scales Y to roughly be in [-0.5, 0.5] (based on 95th and 5th quantile if not in [0, 1]) (TRUE)}

\item{max_approx}{Target approximation ratio for selecting graduated optimisation step size (1.2)}

\item{beta_max}{Stopping sigmoid steepness (25)}

\item{beta_start_max}{Maximum beta-step during the initialisation (1.0)}

\item{max_iterations}{Maximum number of OWL-QN steps per graduated optimisation step (250)}
}
\value{
slise object (coefficients, subset, value, X, Y, lambda, epsilon, scaled, alpha, x, y)
}
\description{
SLISE Black Box Explainer
Use SLISE for explaining predictions made by a black box.
}
\examples{
X <- matrix(rnorm(200), 100, 2)
Y <- rnorm(100)
index <- 10
model <- slise.explain(X, Y, index)
}
