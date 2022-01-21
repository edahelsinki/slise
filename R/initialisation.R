

#' OLS solver that falls back to an optimisation if ncol(X) is huge
#' Also supports LASSO via optimisation
#'
#' @param X data matrix
#' @param Y response vector
#' @param weight weight vector (default: NULL)
#' @param lambda LASSO regularisation (default: 0)
#' @param max_iterations if ncol(X) is huge, then ols is replaced with optimisation (default:300)
#'
#' @return coefficient vector
#'
#' @importFrom stats lm.wfit
#' @importFrom stats .lm.fit
#'
fast_ols <- function(X, Y, weight = NULL, lambda = 0, max_iterations = 300) {
    # If the number of dimensions is very large, don't use the exact OLS solver
    if (lambda > 0 || ncol(X) > max_iterations * 20) {
        # 20 comes from the number of linesearch steps in lbfgs
        if (is.null(weight)) {
            loss <- function(alpha) sum((X %*% alpha - Y)^2) / 2
            grad <- function(alpha) colSums(c(X %*% alpha - Y) * X)
        } else {
            loss <- function(alpha) sum((X %*% alpha - Y)^2 * weight) / 2
            grad <- function(alpha) colSums((c(X %*% alpha - Y) * weight) * X)
        }
        lbfgs::lbfgs(
            loss,
            grad,
            rep(0, ncol(X)),
            invisible = TRUE,
            max_iterations = max_iterations,
            orthantwise_c = lambda
        )$par
    } else if (is.null(weight)) {
        .lm.fit(X, Y)$coefficients
    } else {
        lm.wfit(X, Y, weight)$coefficients
    }
}


#' Initialise the graduated optimisation with a LASSO solution
#'
#
#' @param X data matrix
#' @param Y response vector
#' @param weight weight vector (default: NULL)
#' @param lambda1 L1 regularisation (default: 0)
#' @param max_iterations if ncol(X) is huge, then ols is replaced with optimisation (default:300)
#' @param ... unused parameters
#'
#' @return list(alpha, beta)
#' @export
#'
slise_initialisation_lasso <- function(X, Y, weight = NULL, lambda1 = 0, max_iterations = 300, ...) {
    return(list(alpha = fast_ols(X, Y, weight, lambda1, max_iterations), beta = 0))
}


#' Initialise the graduated optimisation with an "Ordinary Least Squares" solution
#'
#
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param weight weight vector (default: NULL)
#' @param beta_max the maximum starting sigmoid steepness (default: 20/epsilon^2)
#' @param max_approx the target approximation ratio (default: 1.15)
#' @param max_iterations if ncol(X) is huge, then ols is replaced with optimisation (default:300)
#' @param beta_max_init the maximum sigmoid steepness in the initialisation
#' @param ... unused parameters
#'
#' @return list(alpha, beta)
#' @export
#'
slise_initialisation_ols <- function(X,
                                     Y,
                                     epsilon,
                                     weight = NULL,
                                     beta_max = 20 / epsilon^2,
                                     max_approx = 1.15,
                                     max_iterations = 300,
                                     beta_max_init = 2.5 / epsilon^2,
                                     ...) {
    beta_max <- min(beta_max_init, beta_max)
    alpha <- fast_ols(X, Y, weight, 0, max_iterations)
    res <- (Y - X %*% alpha)^2
    beta <- next_beta(res, epsilon^2, 0, weight, beta_max, log(max_approx))
    return(list(alpha = alpha, beta = beta))
}


#' Initialise the graduated optimisation with a zero-vector
#'
#
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param weight weight vector (default: NULL)
#' @param beta_max the maximum starting sigmoid steepness (default: 20/epsilon^2)
#' @param max_approx the target approximation ratio (default: 1.15)
#' @param beta_max_init the maximum sigmoid steepness in the initialisation
#' @param ... unused parameters
#'
#' @return list(alpha, beta)
#' @export
#'
slise_initialisation_zeros <- function(X,
                                       Y,
                                       epsilon,
                                       weight = NULL,
                                       beta_max = 20 / epsilon^2,
                                       max_approx = 1.15,
                                       beta_max_init = 2.5 / epsilon^2,
                                       ...) {
    beta_max <- min(beta_max_init, beta_max)
    alpha <- c(rep(0, ncol(X)))
    beta <- next_beta(Y^2, epsilon^2, 0, weight, beta_max, log(max_approx))
    return(list(alpha = alpha, beta = beta))
}


# Create a candidate for slise_initialisation_candidates
.create_candidate <- function(X, Y, weight = NULL, pca_treshold = 10, max_iterations = 300) {
    if (ncol(X) <= pca_treshold) {
        subset <- sample.int(nrow(X), max(3, ncol(X) + 1), FALSE, weight)
        fast_ols(X[subset, , drop = FALSE], Y[subset])
    } else {
        subset <- sample.int(nrow(X), pca_treshold + 1, FALSE, weight)
        X <- X[subset, , drop = FALSE]
        pca <- simple_pca(X, pca_treshold)
        pca %*% fast_ols(X %*% pca, Y[subset], max_iterations = max_iterations)
    }
}

#' Initialise the graduated optimisation by sampling candidates
#'
#' The procedure starts with creating num_init subsets of size d.
#' For each subset a linear model is fitted and the model that has
#' the smallest loss is selected.
#'
#' The chance that one of these subsets contains only "clean" data is:
#'   $$ 1-(1-(1-noise_fraction)^d)^num_init $$
#' This means that high-dimensional data (large d) can cause issues,
#' which is solved by using PCA (allows for sampling smaller subsets
#' than d).
#'
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param weight weight vector (default: NULL)
#' @param beta_max the maximum sigmoid steepness (default: 20/epsilon^2)
#' @param num_init the number of initial subsets to generate (default: 500)
#' @param max_approx the target approximation ratio (default: 1.15)
#' @param beta_max_init the maximum sigmoid steepness in the initialisation
#' @param pca_treshold the maximum number of columns without using PCA (default: 10)
#' @param max_iterations if ncol(X) is huge, then ols is replaced with optimisation (default:300)
#' @param ... unused parameters
#'
#' @return list(alpha, beta)
#' @export
#'
slise_initialisation_candidates <- function(X,
                                            Y,
                                            epsilon,
                                            weight = NULL,
                                            beta_max = 20 / epsilon^2,
                                            max_approx = 1.15,
                                            num_init = 500,
                                            beta_max_init = 2.5 / epsilon^2,
                                            pca_treshold = 10,
                                            max_iterations = 300,
                                            ...) {
    beta_max <- min(beta_max_init, beta_max)
    max_approx <- log(max_approx)
    epsilon <- epsilon^2
    # Initial model (zeros)
    alpha <- c(rep(0, ncol(X)))
    beta <- next_beta(Y^2, epsilon, 0, weight, beta_max, max_approx)
    loss <- loss_smooth_res(alpha, Y^2, epsilon, beta, 0.0, 0.0)
    # Find the candidate with the best loss for the next_beta
    for (i in 2:num_init) {
        model <- .create_candidate(X, Y, weight = NULL, pca_treshold, max_iterations)
        residuals2 <- (Y - X %*% model)^2
        loss2 <- loss_smooth_res(model, residuals2, epsilon, beta, 0, 0)
        if (loss2 < loss) {
            alpha <- model
            beta <- next_beta(residuals2, epsilon, 0, weight, beta_max, max_approx)
            loss <- loss_smooth_res(model, residuals2, epsilon, beta, 0, 0)
        }
    }
    list(alpha = alpha, beta = beta, loss = loss)
}

# Create a candidate for slise_initialisation_candidates2
.create_candidate2 <- function(X, Y, weight = NULL, max_iterations = 300) {
    subset <- sample.int(nrow(X), 3, FALSE, weight)
    X <- X[subset, , drop = FALSE]
    Y <- Y[subset]
    fast_ols(X, Y, NULL, .Machine$double.eps * 2, max_iterations)
}

#' Initialise the graduated optimisation by sampling candidates
#'
#' The procedure starts with creating num_init subsets of size d.
#' For each subset a linear model is fitted and the model that has
#' the smallest loss is selected.
#'
#' The chance that one of these subsets contains only "clean" data is:
#'   $$ 1-(1-(1-noise_fraction)^d)^num_init $$
#' This means that high-dimensional data (large d) can cause issues,
#' which is solved by using LASSO-regularisation (which enables fitting
#' of linear models with smaller subsets than d).
#'
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param weight weight vector (default: NULL)
#' @param beta_max the maximum sigmoid steepness (default: 20/epsilon^2)
#' @param num_init the number of initial subsets to generate (default: 400)
#' @param max_approx the target approximation ratio (default: 1.15)
#' @param beta_max_init the maximum sigmoid steepness in the initialisation
#' @param max_iterations if ncol(X) is huge, then ols is replaced with optimisation (default:300)
#' @param ... unused parameters
#'
#' @return list(alpha, beta)
#' @export
#'
slise_initialisation_candidates2 <- function(X,
                                             Y,
                                             epsilon,
                                             weight = NULL,
                                             beta_max = 20 / epsilon^2,
                                             max_approx = 1.15,
                                             num_init = 500,
                                             beta_max_init = 2.5 / epsilon^2,
                                             max_iterations = 300,
                                             ...) {
    beta_max <- min(beta_max_init, beta_max)
    max_approx <- log(max_approx)
    epsilon <- epsilon^2
    # Initial model (zeros)
    alpha <- c(rep(0, ncol(X)))
    beta <- next_beta(Y^2, epsilon, 0, weight, beta_max, max_approx)
    loss <- loss_smooth_res(alpha, Y^2, epsilon, beta, 0.0, 0.0)
    # Find the candidate with the best loss for the next_beta
    for (i in 2:num_init) {
        model <- .create_candidate2(X, Y, weight = NULL, max_iterations)
        residuals2 <- (Y - X %*% model)^2
        loss2 <- loss_smooth_res(model, residuals2, epsilon, beta, 0, 0)
        if (loss2 < loss) {
            alpha <- model
            beta <- next_beta(residuals2, epsilon, 0, weight, beta_max, max_approx)
            loss <- loss_smooth_res(model, residuals2, epsilon, beta, 0, 0)
        }
    }
    list(alpha = alpha, beta = beta, loss = loss)
}