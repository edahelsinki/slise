# This script contains some data preprocessing (scaling, centering, et.c.)

#' Robust Scale
#' A scale that can handle zero variance
#'
#' @param x the vector/matrix to normalise
#' @param center Should constant columns be centered (TRUE)
#' @param scale Should constant columns be scaled (TRUE)
#' @param remove_constant Should constant columns be removed (TRUE)
#'
#' @return a list(scaled, center, scale, mask)
#'
scale_robust <- function(x, center=TRUE, scale = TRUE, remove_constant = TRUE) {
    if (is.null(dim(x))) dim(x) <- c(length(x), 1)
    if (center) {
        meanx <- colMeans(x)
        x <- sweep(x, 2, meanx, "-")
    }
    else {
        meanx <- rep(0, ncol(x))
    }
    if (scale) {
        stddv <- sqrt(colSums(x ^ 2) / (nrow(x) - 1))
        if (remove_constant && any(stddv == 0)) {
            mask <- stddv != 0
            stddv <- stddv[mask]
            meanx <- meanx[mask]
            scaled <- sweep(x[, mask, drop = FALSE], 2, stddv, "/")
        }
        else {
            stddv[stddv == 0] <- 1
            scaled <- sweep(x, 2, stddv, "/")
            mask <- NULL
        }
        list(scaled = scaled, center = meanx, scale = stddv, mask = mask)
    }
    else {
        if (remove_constant) {
            mask <- colSums(if(center) x^2 else sweep(x, 2, colMeans(x, na.rm=TRUE), "-")^2) != 0
            if (sum(mask, na.rm = TRUE) < length(mask)) {
                meanx <- meanx[mask]
                x <- x[, mask, drop = FALSE]
            }
            else mask <- NULL
        }
        else mask <- NULL
        list(scaled = x, center = meanx, scale = rep(1, length(meanx)), mask = mask)
    }
}

#' Scale a vector to a range of approx one
#'
#' @param x the vector to normalise
#'
#' @return a list(scaled, center, scale)
#'
scale_one_range <- function(x) {
    qs <- stats::quantile(x, c(0.05, 0.95), names = FALSE)
    if (qs[1] >= 0 && qs[2] <= 1 && qs[1] < 0.5 && qs[2] > 0.5) {
        list(scaled = x - 0.5, scale = 1, center = 0.5)
    } else {
        m <- mean(x)
        qs <- qs - m
        s <- abs(qs[2] - qs[1])
        if (s == 0) s <- 1
        list(scaled = (x - m) / s, scale = s, center = m)
    }
}

#' Create a scale list without any changes
#'
#' @param x the vector to (not) scale
#'
#' @return a list(scaled = x, center = 0, scale = 1)
#'
scale_identity <- function(x) list(scaled = x, scale = rep(1, length(x)), center = rep(0, length(x)))

#' SLISE Data Preprocessing
#' Scales data and optionally adds intercept
#'
#' @param X the data matrix
#' @param Y the response vector
#' @param scale should the data be (columnwise) normalised (FALSE)
#' @param intercept should an intercept column be added (FALSE)
#' @param logit_tr should the response be logit-transformed (FALSE)
#' @param scale_y should the response be scaled (FALSE)
#'
#' @return list(X, Y, scaling_functions...)
#'
data_preprocess <- function(X, Y, scale=FALSE, intercept=FALSE, logit_tr=FALSE, scale_y = TRUE) {
    if (logit_tr) {
        Y <- logit(Y)
    }
    scx <- scale_robust(X, scale = scale, center = scale)
    scy <- if (scale_y) scale_one_range(Y) else scale_identity(Y)
    X <- scx$scaled
    Y <- scy$scaled
    if (intercept) X <- cbind(rep(1, nrow(X)), X)
    list(
        X = X,
        Y = Y,
        scale_x = function(x) {
            if (!is.null(scx$mask))
                x <- x[scx$mask]
            if (length(x) > length(scx$center))
                sweep(sweep(x, 2, scx$center, "-"), 2, scx$scale, "/")
            else
                (x - scx$center) / scx$scale
        },
        scale_y = function(y) {
            if (logit_tr) y <- logit(y)
            (y - scy$center) / scy$scale
        },
        scale_alpha = function(alpha) {
            if (!is.null(scx$mask)) {
                if (length(alpha) > length(scx$mask)) {
                    inter <- alpha[[1]]
                    alpha <- alpha[-1]
                }
                alpha <- alpha[scx$mask]
            } else if (length(alpha) > length(scx$center)) {
                inter <- alpha[[1]]
                alpha <- alpha[-1]
            } else
                inter <- 0
            inter <- (inter - scy$center + sum(alpha * scx$center)) / scy$scale
            alpha <- alpha / scy$scale * scx$scale
            c(inter, alpha)
        },
        unscale_alpha = function(alpha) {
            if (length(alpha) == length(scx$center) + 1) {
                inter <- alpha[[1]]
                alpha <- alpha[-1]
            } else {
                inter <- 0 #Always returns with intercept
            }
            if (length(alpha) != length(scx$center))
                stop(paste(length(alpha), "!=", length(scx$center)))
            inter <- (inter - sum(alpha * scx$center / scx$scale)) * scy$scale + scy$center
            alpha <- alpha / scx$scale * scy$scale
            if (is.null(scx$mask))
                c(inter, alpha)
            else {
                a2 <- rep(0, length(scx$mask))
                a2[scx$mask] <- alpha
                c(inter, a2)
            }
        },
        expand_alpha = function(alpha) {
            if (is.null(scx$mask))
                alpha
            else if (length(alpha) == length(scx$center) + 1) {
                a2 <- rep(0, length(scx$mask))
                a2[scx$mask] <- alpha[-1]
                c(alpha[[1]], a2)
            } else {
                a2 <- rep(0, length(scx$mask))
                a2[scx$mask] <- alpha
                a2
            }
        },
        unscale_y = function(y) {
            y <- y * scy$scale + scy$center
            if (logit_tr) sigmoid(y) else y
        }
    )
}

#' SLISE Data Preprocessing without changing anything
#'
#' @param X the data matrix
#' @param Y the response vector
#'
#' @return list(X, Y, scaling_functions...)
#'
data_identity <- function(X, Y) {
    list(
        X = X,
        Y = Y,
        scale_x = function(x) x,
        scale_y = function(y) y,
        scale_alpha = function(alpha) alpha,
        unscale_alpha = function(alpha) alpha,
        expand_alpha = function(alpha) alpha,
        unscale_y = function(y) y
    )
}


#' Shift the data to be local around a point
#'
#' @param X data matrix
#' @param Y response vector
#' @param x local data point
#' @param y local response
#'
#' @return list(X = X - x, Y = Y - y, scaling_functions...)
#'
data_local <- function(X, Y, x, y=NULL) {
    if (is.null(y)) {
        # x is index
        y <- Y[[x]]
        x <- X[x,]
    }
    X <- sweep(X, 2, x)
    Y <- Y - y
    list(
        X = X,
        Y = Y,
        scale_x = function(z) z - x,
        scale_y = function(z) z - y,
        unscale_alpha = function(alpha) c(y - sum(x * alpha), alpha)
    )
}
