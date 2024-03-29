% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimisation.R
\name{next_beta}
\alias{next_beta}
\title{Find the next beta according to:
  ¤ approximation_ratio(alpha, beta_old, beta_new) == max_approx
  ¤ beta_new >= beta_old + min_increase
  ¤ beta_new <= beta_max}
\usage{
next_beta(
  residuals2,
  epsilon2,
  beta = 0,
  weight = NULL,
  beta_max = 20/epsilon2,
  log_max_approx = log(1.15),
  beta_min_increase = (beta_max + beta) * 5e-04
)
}
\arguments{
\item{residuals2}{squared residuals}

\item{epsilon2}{squared error tolerance}

\item{beta}{current sigmoid steepness}

\item{weight}{weight vector (default: NULL)}

\item{beta_max}{max sigmoid steepnsess}

\item{log_max_approx}{logarithm of the approximation ratio target for increasing beta}

\item{beta_min_increase}{minimum beta step}
}
\value{
beta_new
}
\description{
Find the next beta according to:
  ¤ approximation_ratio(alpha, beta_old, beta_new) == max_approx
  ¤ beta_new >= beta_old + min_increase
  ¤ beta_new <= beta_max
}
