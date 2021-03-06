% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{print.slise}
\alias{print.slise}
\title{Print the robust regression or explanation from slise}
\usage{
\method{print}{slise}(slise, ..., title = "SLISE")
}
\arguments{
\item{slise}{The slise object}

\item{...}{not used}

\item{title}{(Optional) The title of the result}
}
\description{
Print the robust regression or explanation from slise
}
\examples{
data <- matrix(rnorm(200), 100, 2)
response <- rnorm(100)
slise <- slise.fit(data, response, epsilon=0.1)
print(slise)
}
