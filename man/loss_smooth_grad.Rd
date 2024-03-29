% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimisation.R
\name{loss_smooth_grad}
\alias{loss_smooth_grad}
\title{Smooth Loss Gradient
Gradient for the smooth loss function}
\usage{
loss_smooth_grad(
  alpha,
  X,
  Y,
  epsilon,
  beta,
  lambda1 = 0,
  lambda2 = 0,
  weight = NULL
)
}
\arguments{
\item{alpha}{The vector to calculate loss-gradient for}

\item{X}{The data matrix}

\item{Y}{The response vector}

\item{epsilon}{The acceptable error}

\item{beta}{The steepness of the sigmoid}

\item{lambda1}{The L1 regulariser (default: 0)}

\item{lambda2}{The L2 regulariser (default: 0)}

\item{weight}{weight vector (default: NULL)}
}
\value{
The gradients for alpha
}
\description{
Smooth Loss Gradient
Gradient for the smooth loss function
}
