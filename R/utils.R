# This script contains some utility functions

#' sigmoid function
#'
#' @param x vector of real values
#'
#' @return sigmoid(x)
#'
sigmoid <- function(x) 1 / (1 + exp(-x))

#' derivative of sigmoid function
#'
#' @param x vector of real values
#'
#' @return Derivative of sigmoid(x).
#'
dsigmoid <- function(x) {
    s <- sigmoid(x)
    s * (1 - s)
}

#' log-sigmoid function
#'
#' @param x  vector of real values
#'
#' @return log(sigmoid(x))
#'
log_sigmoid <- function(x) ifelse(x >= 0, -log(1 + exp(-x)), x - log(1 + exp(x)))

#' derivative of log-sigmoid function
#'
#' @param x vector of real values
#'
#' @return Derivative of log(sigmoid(x)).
#'
dlog_sigmoid <- function(x) 1 - sigmoid(x)


#' Which min n
#' Get the indecies of the n smallest values using partial sort
#'
#' @param x vector
#' @param n the number of indices
#'
#' @return vector of indecies
#'
which_min_n <- function(x, n) which(x <= sort(x, partial = n)[n])[1:n]


#' Sparsity
#' Count the non-zero coefficients
#'
#' @param x vector
#' @param treshold threshold for zero
#'
#' @return number of non-zero values
#'
sparsity <- function(x, treshold = .Machine$double.eps) sum(abs(x) > treshold)

#' Computes log(sum(exp(x))) in a numerically robust way.
#'
#' @param x vector of length n
#'
#' @return log(sum(exp(x))).
#'
log_sum <- function(x) {
    xmax <- max(x)
    xmax + log(sum(exp(x - xmax)))
}

#' Computes log(sum(exp(x) * y)),
#' or log(sum(exp(x))) if all(y == 0),
#' in a numerically robust way.
#'
#' @param x vector of length n
#' @param y multiplier
#'
#' @return log(sum(exp(x))).
#'
log_sum_special <- function(x, y) {
    xmax <- max(x)
    xexp <- exp(x - xmax)
    xsum <- sum(xexp * y)
    if (xsum == 0) xsum <- sum(xexp)
    xmax + log(xsum)
}

#' Computes the logits from probabilities
#'
#' @param p probability (vector)
#' @param stab limit p to [stab, 1-stab] for numerical stability
#'
#' @return log(p / (1 - p))
#'
limited_logit <- function(p, stab = 0.001) {
    p <- pmin(1.0 - stab, pmax(stab, p))
    log(p / (1.0 - p))
}

# Checks if the object has the attribute
hasattr <- function(object, attribute) {
    !is.null(attr(object, attribute))
}

# A variant of `signif` that gives "" in case of zero
signif2 <- function(x, num = 5) {
    ifelse(abs(x) < .Machine$double.eps, "", signif(x, num))
}

# Check if a package is installed
check_package <- function(pack) {
    if (!requireNamespace(pack, quietly = TRUE)) {
        stop(paste0("Package \"", pack, "\" needed for the function to work. Please install it."), call. = FALSE)
    }
}

#' Creates a named list where the names are taken from the input variables
#'
#' NOTE: only supports arguments, not keyword arguments
#'
#' @param ... list elements
#'
#' @return named list of elements
#'
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' a <- 1
#' b <- 2
#' auto_named_list(a, b)$a == 1
#' auto_named_list(a, b)$b == 2
auto_named_list <- function(...) setNames(list(...), substitute(alist(...)))