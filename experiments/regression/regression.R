suppressPackageStartupMessages({
    library(glmnet)
    library(robustbase)
    library(robustHD)
    library(conquer)
    library(reticulate)
    library(MTE)
    library(R.utils) # For timeout
})


get_regression_methods <- function(add_random = FALSE, add_mean = FALSE) {
    methods <- list(
        slise = list(
            name = "SLISE",
            latex = "\\slise",
            train = function(X, Y, epsilon, lambda = 0, ...) {
                coef <- slise.fit(X, Y, epsilon, lambda, ...)$coefficients
                rr.create(coef, X, Y, epsilon)
            },
            shape = 20,
            size = 2,
            linetype = 1,
            color = "#ed9411"
        ),
        # fastlts = list(
        #     name = "Fast LTS",
        #     latex = "\\fastlts",
        #     train = function(X, Y, epsilon, lambda = 0, ...) {
        #         mod <- ltsReg(X, Y, alpha = 0.5, intercept = TRUE, mcd = FALSE)
        #         rr.create(mod$coefficients, X, Y, epsilon)
        #     }
        # ),
        sparselts = list(
            name = "Sparse LTS",
            latex = "\\sparselts",
            train = function(X, Y, epsilon, lambda = 0, ..., size = 0.5) {
                coef <- sparseLTS(
                    X, Y, lambda / length(Y),
                    normalize = FALSE, intercept = TRUE,
                    alpha = size, ncores = getOption("mc.cores", NA)
                )$coefficients
                rr.create(coef, X, Y, epsilon)
            },
            shape = 1,
            size = 1,
            linetype = 4,
            color = "#8877bb"
        ),
        # mm = list(
        #     name = "MM-Estimator",
        #     latex = "\\mmest",
        #     train = function(X, Y, epsilon, lambda = 0, ...) {
        #         library(MASS)
        #         mod <- rlm(Y ~ 1 + X, init = "lts", psi = psi.bisquare, method = "MM")
        #         rr.create(mod$coefficients, X, Y, epsilon)
        #     }
        # ),
        smdm = list(
            name = "SMDM",
            latex = "\\smdm",
            train = function(X, Y, epsilon, lambda = 0, ...) {
                ng <- max(400, as.integer(ncol(X) * 1.5))
                g <- min(5, as.integer((nrow(X) - 1) / (ncol(X) * 1.5)))
                mod <- lmrob(Y ~ 1 + X, control = lmrob.control(n.group = ng, groups = g, setting = "KS2014"))
                rr.create(mod$coefficients, X, Y, epsilon)
            },
            shape = 15,
            size = 1,
            linetype = 5,
            color = "#66a61e"
        ),
        # mmlasso = list(
        #     name = "MM Lasso",
        #     latex = "\\mmlasso",
        #     train = function(X, Y, epsilon, lambda = 0, ...) {
        #         library(pense)
        #         res <- as.vector(pensem(X, Y, alpha=1, lambda = max(1e-8, lambda),
        #             ncores = getOption("mc.cores", 4L))$coefficients)
        #         rr.create(res, X, Y, epsilon)
        #     }
        # ),
        # quantreg = list(
        #     name = "Quantile Regression",
        #     latex = "\\quantreg",
        #     train = function(X, Y, epsilon, lambda = 0, ...) {
        #         library(quantreg)
        #         mod <- rq(Y ~ 1 + X, 0.5, ...)
        #         rr.create(mod$coefficients, X, Y, epsilon)
        #     }
        # ),
        conquer = list(
            name = "Conquer",
            latex = "\\conquer",
            train = function(X, Y, epsilon, lambda = 0, ...) {
                mod <- conquer(X, Y, 0.5, ...)
                rr.create(mod$coeff, X, Y, epsilon)
            },
            shape = 2,
            size = 1,
            linetype = 3,
            color = "#e7298a"
        ),
        # ladlasso = list(
        #     name = "LAD Lasso",
        #     latex = "\\ladlasso",
        #     train = function(X, Y, epsilon, lambda = 0, ...) {
        #         res <- LAD(Y, X, TRUE)
        #         if (lambda > 0) {
        #             res <- LADlasso(Y, X,
        #                 beta.ini = res,
        #                 lambda = lambda / nrow(X),
        #                 adaptive = FALSE,
        #                 intercept = TRUE
        #             )$beta
        #         }
        #         rr.create(res, X, Y, epsilon)
        #     },
        #     shape = 6,
        #     size = 1,
        #     linetype = 2,
        #     color = "#1b9e77"
        # ),
        mtelasso = list(
            name = "MTE Lasso",
            latex = "\\mtelasso",
            train = function(X, Y, epsilon, lambda = 0, ...) {
                res <- LAD(Y, X, TRUE)
                MTElasso(Y, X,
                    p = 1,
                    beta.ini = res,
                    lambda = lambda / nrow(X),
                    adaptive = FALSE,
                    intercept = TRUE
                )$beta
                rr.create(res, X, Y, epsilon)
            },
            shape = 6,
            size = 1,
            linetype = 2,
            color = "#1b9e77"
        ),
        ransac = list(
            name = "RANSAC",
            latex = "\\ransac",
            train = function(X, Y, epsilon, lambda = 0, ..., max_trials = 20000) {
                ransac <- reticulate::import("sklearn")$linear_model$RANSACRegressor
                model <- ransac(residual_threshold = epsilon, max_trials = max_trials)
                model$fit(X, Y)
                coef <- c(model$estimator_$intercept_, model$estimator_$coef_)
                rr.create(coef, X, Y, epsilon)
            },
            shape = 10,
            size = 1,
            linetype = 6,
            color = "#a6761d"
        ),
        lasso = list(
            name = "Lasso",
            latex = "\\lasso",
            train = function(X, Y, epsilon, lambda = 0, ..., alpha = 1) {
                coef <- as.vector(predict(glmnet(
                    X, Y,
                    lambda = lambda / nrow(X), alpha = alpha
                ), type = "coefficients"))
                rr.create(coef, X, Y, epsilon)
            },
            shape = 4,
            size = 1,
            linetype = 1,
            color = "#e6ab02"
        )
    )
    if (add_random) {
        methods$random <- list(
            name = "Random",
            latex = "\\textsc{random}",
            train = function(X, y, ...) structure(list(y = y), class = "rrr"),
            shape = 15,
            size = 2,
            linetype = 1,
            color = "#000000"
        )
    }
    if (add_mean) {
        methods$mean <- list(
            name = "Mean",
            latex = "\\textsc{mean}",
            train = function(X, y, ...) structure(list(y = mean(y)), class = "rrr"),
            shape = 15,
            size = 2,
            linetype = 1,
            color = "#000000"
        )
    }
    for (m in names(methods)) {
        methods[[m]]$tag <- m
    }
    methods
}

get_nested <- function(list, prop) {
    res <- c(sapply(list, `[[`, prop))
    names(res) <- c(sapply(list, `[[`, "name"))
    res
}

rr.create <- function(coef, X, Y, epsilon) {
    coef[is.na(coef)] <- 0.0
    res <- c(X %*% coef[-1] + coef[1] - Y)
    structure(list(
        coef = coef,
        X = X,
        loss_mse = mean(res^2),
        loss_mae = mean(abs(res)),
        loss_slise = loss_sharp_res(coef, res^2, epsilon^2)
    ), class = "rr")
}

predict.rr <- function(object, newdata = NULL, ...) {
    if (is.null(newdata)) {
        newdata <- object$X
    } else {
        newdata <- as.matrix(newdata)
    }
    c(newdata %*% object$coef[-1] + object$coef[1])
}

with_timelimit <- function(fn, timelimit, desc, ...) {
    tryCatch(
        expr = withTimeout(fn(...), timeout = timelimit),
        TimeoutException = function(ex) {
            cat("\n", "Timeout in:", desc, "\n")
            NULL
        },
        error = function(e) {
            cat("\n", "Error in:", desc, "\n")
            print(e)
            NULL
        }
    )
}

is.rr <- function(object) {
    "rr" %in% class(object)
}

is.rrr <- function(object) {
    "rrr" %in% class(object)
}

predict.rrr <- function(object, newdata = NULL, ...) {
    if (is.null(newdata)) {
        sample(object$y, replace = TRUE)
    } else {
        sample(object$y, nrow(newdata), replace = TRUE)
    }
}