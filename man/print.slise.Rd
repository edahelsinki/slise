% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{print.slise}
\alias{print.slise}
\title{Print the robust regression or explanation from slise}
\usage{
\method{print}{slise}(x, num_vars = 10, labels = NULL, ...)
}
\arguments{
\item{x}{The slise object}

\item{num_vars}{Minimum number of variables to show without filtering (default: 10)}

\item{labels}{Name of y or class labels}

\item{...}{Ignored additional parameters}
}
\value{
invisible(x)
}
\description{
Print the robust regression or explanation from slise
}
\examples{
X <- matrix(rnorm(30), 15, 2)
Y <- runif(15, 0, 1)
print(slise.fit(X, Y, epsilon = 0.1))
}
