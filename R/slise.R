# This script contains the SLISE functions (slise.fit and slise.explain)


#' Use SLISE for robust regression.
#'
#' It is highly recommended that you normalise the data,
#' either before using SLISE or by setting normalise = TRUE.
#'
#' @param X Matrix of independent variables
#' @param Y Vector of the response variable
#' @param epsilon Error tolerance
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight Optional weight vector (default: NULL)
#' @param intercept Should an intercept be added (default: TRUE)
#' @param normalise Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)
#' @param initialisation Function that gives the initial alpha and beta, or a list containing the initial alpha and beta (default: slise_initialisation_candidates)
#' @param ... Other parameters to the optimiser and initialiser
#' @inheritDotParams graduated_optimisation max_approx beta_max max_iterations debug
#' @inheritDotParams slise_initialisation_candidates num_init beta_max_init pca_treshold
#'
#' @return slise.object
#' @export
#'
#' @examples
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
    data <- slise.preprocess(X, Y, epsilon, NULL, NULL, lambda1, lambda2, weight, intercept, normalise, FALSE)
    # Initialisation
    if (is.list(initialisation)) {
        init <- initialisation
        names(init) <- c("alpha", "beta")
    } else {
        init <- initialisation(data$X, data$Y, epsilon = epsilon, weight = weight, ...)
    }
    # Optimisation
    alpha <- graduated_optimisation(
        init$alpha,
        data$X,
        data$Y,
        epsilon = epsilon,
        beta = init$beta,
        lambda1 = lambda1,
        lambda2 = lambda2,
        weight = weight,
        ...
    )$par
    # Output
    out <- slise.object(
        alpha = alpha,
        X = data$X,
        Y = data$Y,
        epsilon = epsilon,
        lambda1 = lambda1,
        lambda2 = lambda2,
        weight = weight,
        intercept = intercept
    )
    if (normalise) {
        out <- slise.object_unnormalise(out, data$X_orig, data$Y_orig)
    }
    options(matprod_default) # Reset options
    out
}

#' Use SLISE for robust regression (using a formula).
#'
#' It is highly recommended that you normalise the data,
#' either before using SLISE or by setting normalise = TRUE.
#'
#' @param formula formula
#' @param data data for the formula
#' @param epsilon Error tolerance
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight Optional weight vector (default: NULL)
#' @param normalise Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)
#' @param initialisation Function that gives the initial alpha and beta, or a list containing the initial alpha and beta (default: slise_initialisation_candidates)
#' @param ... Other parameters to the optimiser and initialiser
#' @inheritDotParams graduated_optimisation max_approx beta_max max_iterations debug
#' @inheritDotParams slise_initialisation_candidates num_init beta_max_init pca_treshold
#'
#' @return slise.object
#' @export
#'
#' @importFrom stats model.frame
#' @importFrom stats model.response
#' @importFrom stats model.matrix
#'
#' @examples
#' data <- data.frame(y = rnorm(8), a = rnorm(8), b = rnorm(8))
#' model <- slise.formula(y ~ a * b + abs(a), data, 0.1, normalise = TRUE)
slise.formula <- function(formula,
                          data,
                          epsilon,
                          lambda1 = 0,
                          lambda2 = 0,
                          weight = NULL,
                          normalise = FALSE,
                          initialisation = slise_initialisation_candidates,
                          ...) {
    mf <- model.frame(formula, data)
    Y <- model.response(mf)
    X <- model.matrix(mf, data)
    if (colnames(X)[[1]] == "(Intercept)") {
        intercept <- TRUE
        X <- X[, -1, drop = FALSE]
    } else {
        intercept <- FALSE
    }
    slise.fit(X, Y, epsilon, lambda1, lambda2, weight, intercept, normalise, initialisation, ...)
}

#' SLISE for explaining Black box models.
#'
#' It is highly recommended that you normalise the data,
#' either before using SLISE or by setting normalise = TRUE.
#'
#' @param X Matrix of independent variables
#' @param Y Vector of the dependent variable
#' @param epsilon Error tolerance
#' @param x The sample to be explained (or index if y is null)
#' @param y The prediction to be explained (default: NULL)
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight Optional weight vector (default: NULL)
#' @param normalise Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)
#' @param logit Logit transform Y from probabilities to real values (default: FALSE)
#' @param initialisation function that gives the initial alpha and beta, or a list containing the initial alpha and beta (default: slise_initialisation_candidates)
#' @param ... Other parameters to the optimiser and initialiser
#' @inheritDotParams graduated_optimisation max_approx beta_max max_iterations debug
#' @inheritDotParams slise_initialisation_candidates num_init beta_max_init pca_treshold
#'
#' @return slise.object
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
    data <- slise.preprocess(X, Y, epsilon, x, y, lambda1, lambda2, weight, FALSE, normalise, logit)
    # Initialisation
    if (is.list(initialisation)) {
        init <- initialisation
        names(init) <- c("alpha", "beta")
    } else {
        init <- initialisation(data$X_local, data$Y_local, epsilon = epsilon, weight = weight, ...)
    }
    # Optimisation
    alpha <- graduated_optimisation(
        init$alpha,
        data$X_local,
        data$Y_local,
        epsilon = epsilon,
        beta = init$beta,
        lambda1 = lambda1,
        lambda2 = lambda2,
        weight = weight,
        ...
    )$par
    # Output
    out <- slise.object(
        alpha = alpha,
        X = data$X,
        Y = data$Y,
        epsilon = epsilon,
        lambda1 = lambda1,
        lambda2 = lambda2,
        weight = weight,
        logit = logit,
        x = data$x,
        y = data$y
    )
    if (normalise) {
        out <- slise.object_unnormalise(out, data$X_orig, data$Y_orig, data$x_orig, data$y_orig)
    }
    options(matprod_default) # Reset options
    out
}

#' Use SLISE as a Black Box Explainer
#' Use SLISE for explaining predictions made by a black box.
#' BUT with a binary search for sparsity!
#'
#' DEPRECATED: This is a simple binary search, no need for a separate function
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
    .Deprecated("slise.explain_comb")
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

#' Use SLISE as a Black Box Explainer
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

#' Preprocess the data as necessary before running SLISE
#'
#' @param X Matrix of independent variables
#' @param Y Vector of the target variable
#' @param epsilon Error tolerance
#' @param x The sample to be explained (or index if y is null)
#' @param y The prediction to be explained (default: NULL)
#' @param lambda1 L1 regularisation coefficient (default: 0)
#' @param lambda2 L2 regularisation coefficient (default: 0)
#' @param weight Optional weight vector (default: NULL)
#' @param intercept Should an intercept be added (default: TRUE)
#' @param normalise Preprocess X and Y by scaling, note that epsilon is not scaled (default: FALSE)
#' @param logit Logit transform Y from probabilities to real values (default: FALSE)
#'
#' @return list(X_orig, Y_orig, X, Y, x_orig, y_orig, x, y)
slise.preprocess <- function(X,
                             Y,
                             epsilon,
                             x = NULL,
                             y = NULL,
                             lambda1 = 0,
                             lambda2 = 0,
                             weight = NULL,
                             intercept = FALSE,
                             normalise = FALSE,
                             logit = FALSE) {
    # Checks
    stopifnot(epsilon > 0)
    stopifnot(lambda1 >= 0)
    stopifnot(lambda2 >= 0)
    if (any(weight < 0)) stop("Weights must not be negative!")
    # Original data as matrix
    X <- as.matrix(X)
    Y <- c(Y)
    if (logit) {
        Y <- limited_logit(Y)
    }
    X_orig <- X
    Y_orig <- Y
    # Preprocessing
    if (normalise) {
        X <- remove_constant_columns(X)
        X <- scale_robust(X)
        Y <- if (logit) {
            scale_identity(Y)
        } else {
            scale_robust(Y)
        }
    }
    if (intercept) {
        X <- add_intercept_column(X)
    }
    # Explanations
    if (!is.null(x)) {
        if (intercept) stop("Explanations cannot have intercepts")
        if (is.null(y)) {
            # x is an index
            y_orig <- Y_orig[[x]]
            x_orig <- X_orig[x, ]
            y <- Y[[x]]
            x <- X[x, ]
        } else {
            if (logit) y <- limited_logit(y)
            x_orig <- x
            y_orig <- y
            if (normalise) {
                x <- scale_same(x, X)
                y <- scale_same(y, Y)
            }
        }
        # Localise
        X_local <- sweep(X, 2, x)
        Y_local <- Y - y
    } else {
        x_orig <- y_orig <- x <- y <- X_local <- Y_local <- NULL
    }
    auto_named_list(X_orig, Y_orig, X, Y, x_orig, y_orig, x, y, X_local, Y_local)
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
#'
#' @return object with SLISE results
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
                         y = NULL) {
    if (!is.null(x)) {
        if (intercept) {
            alpha[1] <- y - sum(x * alpha[-1])
        } else {
            alpha <- c(y - sum(x * alpha), alpha)
            intercept <- TRUE
        }
        terms <- c(1, x) * alpha
    } else {
        terms <- NULL
    }
    if (intercept && length(alpha) == ncol(X)) {
        X <- remove_intercept_column(X)
    }
    var_names <- colnames(X)
    if (length(var_names) == 0 || is.null(var_names)) {
        var_names <- paste(seq_len(ncol(X)))
    }
    if (intercept) {
        dist <- (c(X %*% alpha[-1] + alpha[[1]]) - Y)^2
        var_names <- c("Intercept", var_names)
    }
    else {
        dist <- (c(X %*% alpha) - Y)^2
    }
    names(alpha) <- var_names
    if (!is.null(terms)) names(terms) <- var_names
    subset <- dist <= epsilon^2
    value <- loss <- loss_sharp_res(alpha, dist, epsilon^2, lambda1, lambda2, weight)
    coefficients <- alpha
    out <- auto_named_list(
        coefficients, alpha, subset, value, loss, terms, X, Y, x, y,
        lambda1, logit, lambda2, epsilon, weight, intercept
    )
    structure(out, class = "slise")
}

#' Turn a `slise.object` result based on normalised data to a `slise.object` result with unnormalised data.
#' The normalised results are retained, but with a 'normalised_' prefix.
#'
#' @param object output from `slise.object`
#' @param X unnormalised data matrix
#' @param Y unnormalised response vector
#' @param x explained item x (default: NULL)
#' @param y explained item y (default: NULL)
#'
#' @return object with SLISE results
#'
slise.object_unnormalise <- function(object, X, Y, x = NULL, y = NULL) {
    cc <- attr(object$X, "constant_columns")
    alpha <- unscale_alpha(object$alpha, object$X, object$Y)
    alpha <- add_constant_columns(alpha, cc + 1)
    out <- slise.object(
        alpha = alpha,
        X = X,
        Y = Y,
        epsilon = object$epsilon * attr(object$Y, "scaled:scale"),
        lambda1 = object$lambda1,
        lambda2 = object$lambda2,
        weight = object$weight,
        intercept = TRUE,
        logit = object$logit,
        x = x,
        y = y
    )
    alpha <- add_constant_columns(object$alpha, cc + 1)
    out$normalised <- out$normalised_alpha <- out$normalised_coefficients <- alpha
    if (!is.null(x)) {
        out$normalised_x <- add_constant_columns(object$x, cc)
        out$normalised_y <- object$y
        out$normalised_terms <- add_constant_columns(object$terms, cc + 1)
    }
    out$normalised_loss <- object$loss
    out$normalised_value <- object$value
    out$normalised_epsilon <- object$epsilon
    out$loss <- object$loss
    out$value <- object$value
    out
}

#' Predict with a SLISE object
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
            stop(sprintf("Different number of columns in the new data %d != %d!", ncol(newdata), ncol(object$X)))
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