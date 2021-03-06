% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimisation.R
\name{next_beta}
\alias{next_beta}
\title{Find the next beta according to:
  ¤ approximation_ratio(alpha, beta_old, beta_new) == max_approx
  ¤ beta_new >= beta_old + min_increase}
\usage{
next_beta(alpha, X, Y, epsilon = 0.1, beta = 0, beta_max = 25,
  max_approx = 1.2, beta_min_increase = beta_max * 5e-04)
}
\arguments{
\item{alpha}{linear model}

\item{X}{data matrix}

\item{Y}{response vector}

\item{epsilon}{error tolerance}

\item{beta}{current sigmoid steepness}

\item{beta_max}{max sigmoid steepnsess}

\item{max_approx}{approximation ratio target for increasing beta}

\item{beta_min_increase}{minimum beta step}
}
\value{
beta_new
}
\description{
Find the next beta according to:
  ¤ approximation_ratio(alpha, beta_old, beta_new) == max_approx
  ¤ beta_new >= beta_old + min_increase
}
