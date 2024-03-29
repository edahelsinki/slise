% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimisation.R
\name{grad_opt_debug}
\alias{grad_opt_debug}
\title{Print debug statement for how the graduated optimisation is going}
\usage{
grad_opt_debug(alpha, X, Y, epsilon, beta, lambda1, lambda2, weight, beta_max)
}
\arguments{
\item{alpha}{linear model}

\item{X}{data matrix}

\item{Y}{response vector}

\item{epsilon}{error tolerance}

\item{beta}{current sigmoid steepness}

\item{lambda1}{L1 coefficients}

\item{lambda2}{L2 coefficients}

\item{weight}{weight vector}

\item{beta_max}{max sigmoid steepness}
}
\description{
Print debug statement for how the graduated optimisation is going
}
