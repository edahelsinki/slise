# This script contains some utility functions

#' sigmoid function
#'
#' @param x vector
#' @return sigmoid(x)
sigmoid <- function(x) 1 / (1 + exp(-x))

#' derivative of sigmoid function
#'
#' @param x vector
#' @return Derivative of sigmoid(x).
dsigmoid <- function(x) {
    s <- sigmoid(x)
    s * (1 - s)
}

#' log-sigmoid function
#'
#' @param x vector
#' @return log(sigmoid(x))
log_sigmoid <- function(x) ifelse(x >= 0, - log(1 + exp(-x)), x - log(1 + exp(x)))

#' derivative of log-sigmoid function
#'
#' @param x vector
#' @return Derivative of log(sigmoid(x)).
dlog_sigmoid <- function(x) 1 - sigmoid(x)


#' Which min n
#' Get the indecies of the n smallest values using partial sort
#'
#' @param x vector
#' @param n the number of indecies
#' @return vector of indecies
which_min_n <- function(x, n) which(x <= sort(x, partial = n)[n])[1:n]


#' Sparsity
#' Count the non-zero coefficients
#'
#' @param x vector
#' @param treshold treshold for approximately zero (0)
#' @return number of non-zero values
sparsity <- function(x, treshold = 0) sum(abs(x) > treshold)

#' Computes log(sum(exp(x))) in a numerically robust way.
#'
#' @param x vector of length n
#' @return log(sum(exp(x))).
log_sum <- function(x) {
    xmax <- max(x)
    xmax + log(sum(exp(x - xmax)))
}

#' Computes the logits from probabilities
#'
#' @param p probability (vector)
#' @param stab limit p to [stab, 1-stab] for numerical stability
#' @return log(p / (1 - p))
logit <- function(p, stab = 0.001) {
    p <- pmin(1.0 - stab, pmax(stab, p))
    log(p / (1.0 - p))
}
