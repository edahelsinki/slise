% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialisation.R
\name{slise_initialisation_candidates}
\alias{slise_initialisation_candidates}
\title{Initialise the graduated optimisation by sampling candidates}
\usage{
slise_initialisation_candidates(
  X,
  Y,
  epsilon,
  weight = NULL,
  beta_max = 20/epsilon^2,
  max_approx = 1.15,
  num_init = 500,
  beta_max_init = 2.5/epsilon^2,
  pca_treshold = 10,
  max_iterations = 300,
  ...
)
}
\arguments{
\item{X}{data matrix}

\item{Y}{response vector}

\item{epsilon}{error tolerance}

\item{weight}{weight vector (default: NULL)}

\item{beta_max}{the maximum sigmoid steepness (default: 20/epsilon^2)}

\item{max_approx}{the target approximation ratio (default: 1.15)}

\item{num_init}{the number of initial subsets to generate (default: 500)}

\item{beta_max_init}{the maximum sigmoid steepness in the initialisation}

\item{pca_treshold}{the maximum number of columns without using PCA (default: 10)}

\item{max_iterations}{if ncol(X) is huge, then ols is replaced with optimisation (default:300)}

\item{...}{unused parameters}
}
\value{
list(alpha, beta)
}
\description{
The procedure starts with creating num_init subsets of size d.
For each subset a linear model is fitted and the model that has
the smallest loss is selected.
}
\details{
The chance that one of these subsets contains only "clean" data is:
  $$ 1-(1-(1-noise_fraction)^d)^num_init $$
This means that high-dimensional data (large d) can cause issues,
which is solved by using PCA (allows for sampling smaller subsets
than d).
}
