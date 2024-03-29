% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialisation.R
\name{slise_initialisation_ols}
\alias{slise_initialisation_ols}
\title{Initialise the graduated optimisation with an "Ordinary Least Squares" solution}
\usage{
slise_initialisation_ols(
  X,
  Y,
  epsilon,
  weight = NULL,
  beta_max = 20/epsilon^2,
  max_approx = 1.15,
  max_iterations = 300,
  beta_max_init = 2.5/epsilon^2,
  ...
)
}
\arguments{
\item{X}{data matrix}

\item{Y}{response vector}

\item{epsilon}{error tolerance}

\item{weight}{weight vector (default: NULL)}

\item{beta_max}{the maximum starting sigmoid steepness (default: 20/epsilon^2)}

\item{max_approx}{the target approximation ratio (default: 1.15)}

\item{max_iterations}{if ncol(X) is huge, then ols is replaced with optimisation (default:300)}

\item{beta_max_init}{the maximum sigmoid steepness in the initialisation}

\item{...}{unused parameters}
}
\value{
list(alpha, beta)
}
\description{
Initialise the graduated optimisation with an "Ordinary Least Squares" solution
}
