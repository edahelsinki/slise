library(devtools)
devtools::load_all()

#' Create Synthetic data
#'
#' @param n number of items
#' @param d number of columns
#' @param num_zero number of irrelevant features
#' @param epsilon
#' @param rnd_unif fraction of uniform noise
#' @param rnd_adver fraction of adversarial noise models (list)
#'
#' @return list(X, Y, alpha, clean)
#'
data_create <- function(n, d, num_zero = floor(d * 0.3), epsilon = 0.1, rnd_unif = 0.0, rnd_adver = rep(0.1, 8)) {
    X <- matrix(rnorm(d * n), n, d)
    X <- sweep(X, 2, rnorm(d))
    alpha <- runif(d + 1, -1, 1)
    if (num_zero > 0) alpha[which_min_n(abs(alpha), num_zero)] <- 0
    Y <- X %*% alpha[-1] + alpha[[1]]
    Y <- Y + rnorm(n, sd = sd(Y) * epsilon / 2)
    clean <- Y
    start <- 1
    for (i in rnd_adver) {
        if (i > 0) {
            size <- floor(n * i)
            a2 <- runif(d + 1, -1, 1)
            mask <- (start + 1):(start + size)
            Y[mask] <- X[mask, ] %*% a2[-1] + a2[[1]] + rnorm(size, sd = sd(Y) * epsilon / 2)
            start <- start + size
        }
    }
    if (rnd_unif > 0) {
        size <- floor(n * rnd_unif)
        Y[(start + 1):(start + size)] <- runif(size, min(Y), max(Y))
    }
    list(X = X, Y = c(Y), alpha = alpha, clean = c(clean))
}