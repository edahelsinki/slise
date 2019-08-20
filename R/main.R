# This script contains the SLISE functions (slise.fit and slise.explain)

#' SLISE Regression
#' Use SLISE for robust regression.
#'
#' @param X matrix of independent variables
#' @param Y vector of the response variable
#' @param epsilon error tolerance (will be scaled to represent a percentage, e.g. 0.1 == 10\%)
#' @param lambda sparsity reguraliser
#' @param ... other parameters to the optimiser
#' @param scale Scale X by mean and standard deviation (FALSE)
#' @param logit Should Y be logit-transformed (recommended for probabilities) (FALSE)
#' @param intercept Should an intercept be added (TRUE)
#' @param max_approx Target approximation ratio for selecting graduated optimisation step size (1.2)
#' @param beta_max Stopping sigmoid steepness (25)
#' @param beta_start_max Maximum beta-step during the initialisation (1.0)
#' @param max_iterations Maximum number of OWL-QN steps per graduated optimisation step (250)
#' @param scale_y Scales Y to roughly be in [-0.5, 0.5] (based on 95th and 5th quantile if not in [0, 1]) (TRUE)
#'
#' @return slise object (coefficients, subset, value, X, Y, lambda, epsilon, scaled, alpha)
#' @export
#'
#' @examples
#' X <- matrix(rnorm(200), 100, 2)
#' Y <- rnorm(100)
#' model <- slise.fit(X, Y)
#' prediction <- predict(model, X)
slise.fit <- function(X, Y, epsilon = 0.1, lambda = 0, ..., scale = FALSE, logit = FALSE, intercept = TRUE, scale_y = TRUE) {
    # Setup
    matprod_default <- options(matprod = "blas") # Use faster math
    X <- as.matrix(X)
    data <- data_preprocess(X, Y, scale = scale, intercept = intercept, logit_tr = logit, scale_y = scale_y)
    # Initialisation
    alpha <- stats::.lm.fit(data$X, data$Y)$coefficients
    beta <- 0
    # Optimisation
    alpha <- graduated_optimisation(alpha, data$X, data$Y, epsilon = epsilon, lambda = lambda, ...)$par
    # Output
    out <- create_slise(alpha, X, Y, epsilon, lambda, data, NULL, NULL)
    options(matprod_default) # Reset options
    out
}

#' SLISE Regression
#' The raw interface for SLISE, initialisation and scaling
#'   (including locality) has to be done in advance.
#' This function essentially wraps graduated_optimisation
#'   and create_slise.
#'
#' @param X matrix of independent variables
#' @param Y vector of the response variable
#' @param alpha Starting alpha
#' @param epsilon error tolerance (will be scaled to represent a percentage, e.g. 0.1 == 10\%)
#' @param lambda sparsity reguraliser
#' @param beta Starting sigmoid steepness
#' @param ... other parameters to the optimiser
#' @param max_approx Target approximation ratio for selecting graduated optimisation step size (1.2)
#' @param beta_max Stopping sigmoid steepness (25)
#' @param max_iterations Maximum number of OWL-QN steps per graduated optimisation step (250)
#'
#' @return slise object (coefficients, subset, value, X, Y, lambda, epsilon, scaled, alpha)
#' @export
#'
#' @examples
#' X <- matrix(rnorm(200), 100, 2)
#' Y <- rnorm(100)
#' model <- slise.raw(X, Y)
slise.raw <- function(X, Y, alpha = rep(0, ncol(X)), epsilon = 0.1, lambda = 0, beta = 0, ...) {
    # Setup
    matprod_default <- options(matprod = "blas") # Use faster math
    X <- as.matrix(X)
    # Optimisation
    alpha <- graduated_optimisation(alpha, X, Y, epsilon = epsilon, lambda = lambda, ...)$par
    # Output
    out <- create_slise(alpha, X, Y, epsilon, lambda, data_identity(X, Y), NULL, NULL)
    options(matprod_default) # Reset options
    out
}

#' SLISE Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#'
#' @param X matrix of independent variables
#' @param Y vector of the dependent variable
#' @param x the sample to be explained (or index if y is null)
#' @param y the prediction to be explained
#' @param epsilon error tolerance (will be scaled to represent a percentage, e.g. 0.1 == 10\%)
#' @param lambda sparsity reguraliser
#' @param ... other parameters to the optimiser
#' @param scale Scale X by mean and standard deviation (FALSE)
#' @param logit Should Y be logit-transformed (recommended for probabilities) (FALSE)
#' @param max_approx Target approximation ratio for selecting graduated optimisation step size (1.2)
#' @param beta_max Stopping sigmoid steepness (25)
#' @param beta_start_max Maximum beta-step during the initialisation (1.0)
#' @param max_iterations Maximum number of OWL-QN steps per graduated optimisation step (250)
#' @param scale_y Scales Y to roughly be in [-0.5, 0.5] (based on 95th and 5th quantile if not in [0, 1]) (TRUE)
#'
#' @return slise object (coefficients, subset, value, X, Y, lambda, epsilon, scaled, alpha, x, y)
#' @export
#'
#' @examples
#' X <- matrix(rnorm(200), 100, 2)
#' Y <- rnorm(100)
#' index <- 10
#' model <- slise.explain(X, Y, index)
slise.explain <- function(X, Y, x, y = NULL, epsilon = 0.1, lambda = 0, ..., scale = FALSE, logit = FALSE, scale_y = TRUE) {
    # Setup
    matprod_default <- options(matprod = "blas") # Use faster math
    X <- as.matrix(X)
    if (all(is.null(y))) {
        y <- Y[[x]]
        x <- X[x, ]
    }
    data <- data_preprocess(X, Y, scale = scale, intercept = FALSE, logit_tr = logit, scale_y = scale_y)
    xs <- data$scale_x(x)
    local <- data_local(data$X, data$Y, xs, data$scale_y(y))
    # Initialisation
    alpha <- stats::.lm.fit(local$X, local$Y)$coefficients
    beta <- 0
    # Optimisation
    alpha <- graduated_optimisation(alpha, local$X, local$Y, epsilon = epsilon, lambda = lambda, beta=beta, ...)$par
    # Output
    out <- create_slise(local$unscale_alpha(alpha), X, Y, epsilon, lambda, data, x = x, y = y, logit = logit)
    options(matprod_default) # Reset options
    out
}

#' SLISE Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#' BUT with a binary search for sparsity!
#'
#' @param ... parameters to slise.explain
#' @param lambda the starting value of the search
#' @param variables number of non-zero coefficients
#' @param iters number of search iterations
#' @param treshold treshold for zero coefficient
#'
#' @return SLISE object
#' @export
#'
#' @examples
#' X <- matrix(rnorm(800), 100, 8)
#' Y <- rnorm(100)
#' index <- 10
#' model <- slise.explain_find(X, Y, index, variables = 4)
slise.explain_find <- function(..., lambda = 5, variables = 4, iters = 10, treshold = 1e-4) {
    lower <- 0
    upper <- -1
    upper_best <- NULL
    lower_best <- NULL
    for (j in 1:iters) {
        slise <- slise.explain(lambda = lambda, ...)
        s <- sparsity(slise$alpha[-1], treshold)
        if (s > variables) {
            lower_best <- slise
            lower <- lambda
        } else {
            upper <- lambda
            upper_best <- slise
        }
        if (upper < 0)
            lambda <- lambda * 2
        else
            lambda <- (upper + lower) * 0.5
    }
    if (!is.null(upper_best) && sparsity(upper_best$alpha[-1], treshold) == variables)
        upper_best
    else if (is.null(lower_best))
        slise <- slise.explain(lambda = lower, ...)
    else
        lower_best
}

#' SLISE Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#' BUT with sparsity from a combinatorial search rather than Lasso!
#'
#' @param X matrix of independent variables
#' @param Y vector of the dependent variable
#' @param x the sample to be explained (or index if y is null)
#' @param y the prediction to be explained
#' @param ... other parameters to slise.explain
#' @param variables the number of non-zero coefficients
#'
#' @return SLISE object
#' @export
#'
#' @examples
#' X <- matrix(rnorm(400), 100, 4)
#' Y <- rnorm(100)
#' index <- 10
#' model <- slise.explain_comb(X, Y, index, variables = 2)
slise.explain_comb <- function(X, Y, x, y=NULL, ..., variables = 4) {
    len <- ncol(X)
    combs <- factorial(len) / factorial(variables) / factorial(len - variables)
    if (combs >= 30)
        warning(sprintf("The combinatorial search will take a long time (requires %d iterations)", combs))
    if (all(is.null(y))) {
        y <- Y[[x]]
        x <- X[x, ]
    }
    res <- utils::combn(1:len, variables, function(s) {
        X2 <- X
        for (i in (1:len)[-s])
            X2[, i] <- x[i]
        slise.explain(X2, Y, x, y, ...)
    }, simplify = FALSE)
    expl <- res[[which.min(sapply(res, function(r) r$value))]]
    expl$X <- X
    expl
}

#' Create a result object for SLISE that is similar to other regression method results
#'
#' @param alpha linear model
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param lambda L1 weight
#' @param data data_preprocess(X, Y)
#' @param ... other variables to add to the SLISE object
#'
#' @return list(coefficients=unscale(alpha), X, Y, scaled=data, lambda, alpha, subset=[r_i<epsilon], value=loss, epsilon, loss, ...)
#'
create_slise <- function(alpha, X, Y, epsilon, lambda = 0, data = NULL, ...) {
    if (is.null(data)) {
        coeff <- alpha
        if (length(alpha) > ncol(X))
            dist <- (c(X %*% alpha[-1] + alpha[[1]]) - Y) ^ 2
        else
            dist <- (c(X %*% alpha) - Y) ^ 2
    } else {
        coeff <- data$unscale_alpha(alpha)
        if (length(alpha) > ncol(data$X))
            dist <- (c(data$X %*% alpha[-1] + alpha[[1]]) - data$Y) ^ 2
        else
            dist <- (c(data$X %*% alpha) - data$Y) ^ 2
    }
    if (is.null(colnames(X))) {
        nams <- paste(1:ncol(X))
    } else {
        nams <- colnames(X)
    }
    if (length(coeff) > ncol(X)) {
        names(coeff) <- c("Intercept", nams)
    } else {
        names(coeff) <- nams
    }
    mask <- dist <= epsilon ^ 2
    loss <- sum(mask * (dist / nrow(X) - epsilon ^ 2)) + lambda * sum(abs(alpha))
    structure(list(coefficients = coeff, X = X, Y = Y, scaled = data,
        lambda = lambda, alpha = alpha, subset = mask, value = loss,
        epsilon = epsilon, loss = loss, ...), class = "slise")
}

#' Predict with a SLISE
#'
#' @param object SLISE object
#' @param newdata data matrix
#' @param ... not used
#'
#' @return prediction vector
#' @export
#'
#' @examples
#' X <- matrix(rnorm(200), 100, 2)
#' Y <- rnorm(100)
#' index <- 10
#' model <- slise.explain(X, Y, index)
#' prediction <- predict(model, X)
predict.slise <- function(object, newdata = NULL, ...) {
    if (is.null(newdata)) {
        newdata <- object$scaled$X
    } else {
        newdata <- as.matrix(newdata)
        newdata <- object$scaled$scale_x(newdata)
        if (length(newdata) <= length(object$alpha))
            dim(newdata) <- c(1, length(newdata))
    }
    if (ncol(newdata) == length(object$alpha)) {
        y <- newdata %*% object$alpha
    } else if (ncol(newdata) == length(object$alpha) - 1) {
        y <- newdata %*% object$alpha[-1] + object$alpha[[1]]
    } else {
        stop("Wrong number of columns in data")
    }
    object$scaled$unscale_y(c(y))
}
