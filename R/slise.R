# This script contains the SLISE functions (slise.fit and slise.explain)


#' SLISE Regression
#' Use SLISE for robust regression.
#'
#' It is highly recommended that you normalise the data,
#' either before using SLISE or by setting normalise = TRUE.
#'
#' @param X matrix of independent variables
#' @param Y vector of the response variable
#' @param epsilon error tolerance
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight optional weight vector, negative values will be truncated to zero (default: NULL)
#' @param intercept Should an intercept be added (default: TRUE)
#' @param normalise Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)
#' @param initialisation function that gives the initial alpha and beta, or a list containing the initial alpha and beta (default: slise_initialisation_candidates)
#' @param ... other parameters to the optimiser and initialiser
#' @inheritDotParams graduated_optimisation max_approx beta_max max_iterations debug
#' @inheritDotParams slise_initialisation_candidates num_init beta_max_init pca_treshold
#'
#' @return slise object (coefficients, subset, value, X, Y, lambda1, lambda2, epsilon, scaled, alpha)
#' @export
#'
#' @examples
#' # Assuming data is a data.frame with the first column containing the response
#' # Further assuming newdata is a similar data.frame with the response missing
#' X <- matrix(rnorm(32), 8, 4)
#' Y <- rnorm(8)
#' model <- slise.fit(X, Y, (max(Y) - min(Y)) * 0.1)
#' predicted <- predict(model, X)
slise.fit <- function(X,
                      Y,
                      epsilon,
                      lambda1 = 0,
                      lambda2 = 0,
                      weight = NULL,
                      intercept = TRUE,
                      normalise = FALSE,
                      initialisation = slise_initialisation_candidates,
                      ...) {
    # Setup
    matprod_default <- options(matprod = "blas") # Use faster math
    X <- as.matrix(X)
    Y <- c(Y)
    X_orig <- X
    Y_orig <- Y
    if (length(weight) > 1) {
        weight <- pmax(weight, 0)
    }
    stopifnot(epsilon > 0)
    # Preprocessing
    if (normalise) {
        X <- remove_constant_columns(X)
        X <- scale_robust(X)
        Y <- scale_robust(Y)
        if (!intercept) {
            stop("Normalisation requires intercept")
        }
    }
    if (intercept) {
        X <- add_intercept_column(X)
    }
    # Initialisation
    if (is.list(initialisation)) {
        init <- initialisation
        names(init) <- c("alpha", "beta")
    } else {
        init <- initialisation(X, Y, epsilon = epsilon, weight = weight, ...)
    }
    # Optimisation
    alpha <- graduated_optimisation(
        init$alpha,
        X,
        Y,
        epsilon = epsilon,
        beta = init$beta,
        lambda1 = lambda1,
        lambda2 = lambda2,
        weight = weight,
        ...
    )$par
    # Output
    if (normalise) {
        alpha2 <- unscale_alpha(alpha, X, Y)
        alpha2 <- add_constant_columns(alpha2, attr(X, "constant_columns") + 1)
        alpha <- add_constant_columns(alpha, attr(X, "constant_columns") + 1)
        out <- slise.object(
            alpha2,
            X_orig,
            Y_orig,
            epsilon * attr(Y, "scaled:scale"),
            lambda1,
            lambda2,
            weight,
            intercept,
            normalised = alpha
        )
    } else {
        out <- slise.object(alpha, X_orig, Y, epsilon, lambda1, lambda2, weight, intercept)
    }
    options(matprod_default) # Reset options
    out
}


#' SLISE Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#'
#' It is highly recommended that you normalise the data,
#' either before using SLISE or by setting normalise = TRUE.
#'
#' @param X matrix of independent variables
#' @param Y vector of the dependent variable
#' @param epsilon error tolerance
#' @param x the sample to be explained (or index if y is null)
#' @param y the prediction to be explained (default: NULL)
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight optional weight vector, negative values will be truncated to zero (default: NULL)
#' @param normalise Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)
#' @param logit Logit transform Y from probabilities to real values (default: FALSE)
#' @param initialisation function that gives the initial alpha and beta, or a list containing the initial alpha and beta (default: slise_initialisation_candidates)
#' @param ... other parameters to the optimiser and initialiser
#' @inheritDotParams graduated_optimisation max_approx beta_max max_iterations debug
#' @inheritDotParams slise_initialisation_candidates num_init beta_max_init pca_treshold
#'
#' @return slise object (coefficients, subset, value, X, Y, lambda1, lambda2, epsilon, scaled, alpha, x, y)
#' @export
#'
#' @examples
#' X <- matrix(rnorm(32), 8, 4)
#' Y <- runif(8, 0, 1)
#' expl <- slise.explain(X, Y, 0.1, 3, lambda1 = 0.01, logit = TRUE)
#' plot(expl, "bar", labels = c("class 1", "class 2"))
slise.explain <- function(X,
                          Y,
                          epsilon,
                          x,
                          y = NULL,
                          lambda1 = 0,
                          lambda2 = 0,
                          weight = NULL,
                          normalise = FALSE,
                          logit = FALSE,
                          initialisation = slise_initialisation_candidates,
                          ...) {
    # Setup
    matprod_default <- options(matprod = "blas") # Use faster math
    X <- as.matrix(X)
    Y <- c(Y)
    if (logit) {
        Y <- limited_logit(Y)
    }
    if (length(weight) > 1) {
        weight <- pmax(weight, 0)
    }
    stopifnot(epsilon > 0)
    X_orig <- X
    Y_orig <- Y
    if (is.null(y)) {
        # x is an index
        y <- Y[[x]]
        x <- X[x, ]
    } else if (logit) {
        y <- limited_logit(y)
    }
    x_orig <- x
    y_orig <- y
    # Preprocessing
    if (normalise) {
        X <- remove_constant_columns(X)
        X <- scale_robust(X)
        Y <- scale_robust(Y)
        x <- scale_same(x, X)
        y <- scale_same(y, Y)
    }
    # Localise
    X <- sweep(X, 2, x)
    Y <- Y - y
    # Initialisation
    if (is.list(initialisation)) {
        init <- initialisation
        names(init) <- c("alpha", "beta")
    } else {
        init <- initialisation(X, Y, epsilon = epsilon, weight = weight, ...)
    }
    # Optimisation
    alpha <- graduated_optimisation(
        init$alpha,
        X,
        Y,
        epsilon = epsilon,
        beta = init$beta,
        lambda1 = lambda1,
        lambda2 = lambda2,
        weight = weight,
        ...
    )$par
    # Output
    if (normalise) {
        alpha2 <- unscale_alpha(alpha, X, Y)
        alpha2 <- add_constant_columns(alpha2[-1], attr(X, "constant_columns"))
        alpha2 <- c(y_orig - sum(x_orig * alpha2), alpha2)
        alpha <- add_constant_columns(alpha, attr(X, "constant_columns"))
        x <- add_constant_columns(x, attr(X, "constant_columns"))
        alpha <- c(y - sum(x * alpha), alpha)
        out <- slise.object(
            alpha2,
            X_orig,
            Y_orig,
            epsilon * attr(Y, "scaled:scale"),
            lambda1,
            lambda2,
            weight,
            TRUE,
            x = x_orig,
            y = y_orig,
            impact = c(1, x_orig) * alpha2,
            logit = logit,
            normalised = alpha,
            normalised_x = x,
            normalised_y = y,
            normalised_impact = c(1, x) * alpha
        )
    } else {
        alpha <- c(y_orig - sum(x_orig * alpha), alpha)
        out <- slise.object(
            alpha,
            X_orig,
            Y_orig,
            epsilon,
            lambda1,
            lambda2,
            weight,
            TRUE,
            x = x_orig,
            y = y_orig,
            impact = c(1, x_orig) * alpha,
            logit = logit
        )
    }
    options(matprod_default) # Reset options
    out
}

#' SLISE Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#' BUT with a binary search for sparsity!
#'
#' @param ... parameters to slise.explain
#' @inheritDotParams slise.explain -lambda1
#' @param lambda1 the starting value of the search
#' @param variables number of non-zero coefficients
#' @param iters number of search iterations
#' @param treshold treshold for zero coefficient
#'
#' @return SLISE object
#' @export
#'
slise.explain_find <- function(..., lambda1 = 5, variables = 4, iters = 10, treshold = 1e-4) {
    lower <- 0
    upper <- -1
    upper_best <- NULL
    lower_best <- NULL
    for (j in 1:iters) {
        slise <- slise.explain(lambda1 = lambda1, ...)
        s <- sparsity(slise$alpha[-1], treshold)
        if (s > variables) {
            lower_best <- slise
            lower <- lambda1
        } else {
            upper <- lambda1
            upper_best <- slise
        }
        if (upper < 0) {
            lambda1 <- lambda1 * 2
        } else {
            lambda1 <- (upper + lower) * 0.5
        }
    }
    if (!is.null(upper_best) && sparsity(upper_best$alpha[-1], treshold) == variables) {
        upper_best
    } else if (is.null(lower_best)) {
        slise <- slise.explain(lambda1 = lower, ...)
    } else {
        lower_best
    }
}

#' SLISE Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#' BUT with sparsity from a combinatorial search rather than Lasso!
#'
#' @param X matrix of independent variables
#' @param Y vector of the dependent variable
#' @param epsilon error tolerance
#' @param x the sample to be explained (or index if y is null)
#' @param y the prediction to be explained
#' @param ... other parameters to slise.explain
#' @inheritDotParams slise.explain -X -Y -x -y -epsilon
#' @param variables the number of non-zero coefficients
#'
#' @return SLISE object
#' @export
#'
#' @importFrom utils combn
#'
slise.explain_comb <- function(X, Y, epsilon, x, y = NULL, ..., variables = 4) {
    if (all(is.null(y))) {
        y <- Y[[x]]
        x <- X[x, ]
    }
    X <- as.matrix(X)
    len <- ncol(X)
    combs <- factorial(len) / factorial(variables) / factorial(len - variables)
    if (combs >= 30) {
        warning(sprintf("The combinatorial search will take a long time (requires %d iterations)", combs))
    }
    res <- combn(1:len, variables, function(s) {
        res <- slise.explain(X[, -s, drop = FALSE], Y, epsilon, x[-s], y, ...)
        res$X <- X
        alpha <- add_constant_columns(res$alpha, s + 1)
        res$alpha <- alpha
        res$coefficients <- alpha
        res
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
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight weight vector (default: NULL)
#' @param intercept does the model have an intercept (default: FALSE)
#' @param logit has the target been logit-transformed (default: FALSE)
#' @param x explained item x (default: NULL)
#' @param y explained item y (default: NULL)
#' @param ... other variables to add to the SLISE object
#'
#' @return list(coefficients=unscale(alpha), X, Y, scaled=data, lambda1, lambda2, alpha, subset=[r_i<epsilon], value=loss, epsilon, loss, weight, ...)
#'
slise.object <- function(alpha,
                         X,
                         Y,
                         epsilon,
                         lambda1 = 0,
                         lambda2 = 0,
                         weight = NULL,
                         intercept = FALSE,
                         logit = FALSE,
                         x = NULL,
                         y = NULL,
                         ...) {
    var_names <- colnames(X)
    if (length(var_names) == 0 && is.null(var_names)) {
        var_names <- paste(1:ncol(X))
    }
    if (intercept) {
        dist <- (c(X %*% alpha[-1] + alpha[[1]]) - Y)^2
        var_names <- c("Intercept", var_names)
    }
    else {
        dist <- (c(X %*% alpha) - Y)^2
    }
    names(alpha) <- var_names
    mask <- dist <= epsilon^2
    loss <- loss_sharp_res(alpha, dist, epsilon^2, lambda1, lambda2, weight)
    structure(list(
        coefficients = alpha, X = X, Y = Y, lambda1 = lambda1, logit = logit,
        lambda2 = lambda2, alpha = alpha, subset = mask, value = loss,
        epsilon = epsilon, loss = loss, weight = weight, intercept = intercept,
        x = x, y = y, ...
    ), class = "slise")
}

#' Predict with a SLISE
#'
#' @param object SLISE object
#' @param newdata data matrix
#' @param ... ignored additional parameters
#' @param logit return the result in logit space if a logit has been applied to Y (default: FALSE)
#'
#' @return prediction vector
#' @export
#'
#' @importFrom stats predict
#'
predict.slise <- function(object, newdata = NULL, ..., logit = FALSE) {
    if (is.null(newdata)) {
        newdata <- object$X
    } else {
        if (is.vector(newdata)) {
            newdata <- t(newdata)
        } else {
            newdata <- as.matrix(newdata)
        }
        if (ncol(newdata) != ncol(object$X)) {
            stop(sprintf("Different number of columns in the new data %d != %d", ncol(newdata), ncol(object$X)))
        }
    }
    if (object$intercept) {
        out <- newdata %*% object$alpha[-1] + object$alpha[1]
    } else {
        out <- newdata %*% object$alpha
    }
    if (!logit && object$logit) {
        sigmoid(out)
    } else {
        out
    }
}