% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.slise_mnist}
\alias{plot.slise_mnist}
\title{Plot the robust regression or explanation from slise as an image}
\usage{
\method{plot}{slise_mnist}(
  slise,
  title = NULL,
  labels = c("Low", "High"),
  partial = FALSE,
  width = floor(sqrt(ncol(slise$X))),
  height = width,
  plots = 1,
  enhance_colours = TRUE,
  ...,
  breaks = NULL
)
}
\arguments{
\item{slise}{The slise object}

\item{title}{The title of the plot (may include a `\%s`, which will be replaced by the prediction)}

\item{labels}{The class labels (vector with two strings: c(y_low, y_high), default: c("Low", "High"))}

\item{partial}{Should the raw ggplot2 objects be returned instead of directly plotting (default: FALSE)}

\item{width}{The width of the image (width * height == ncol(X))}

\item{height}{The height of the image (width * height == ncol(X))}

\item{plots}{The number of plots to split the explanation into (default: 1)}

\item{enhance_colours}{Increse the saturation of the explanation (default: TRUE)}

\item{...}{Ignored parameters}

\item{breaks}{Breaks for the countours, see `ggplot2::stat_contour` (default: NULL)}
}
\value{
ggplot object(s) or plot
}
\description{
Plot the robust regression or explanation from slise as an image
}
