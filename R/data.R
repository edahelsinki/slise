# This script contains some data preprocessing (scaling, centering, et.c.)

add_intercept_column <- function(x) {
    if (is.null(dim(x))) {
        dim(x) <- c(length(x), 1)
    }
    x2 <- cbind(rep(1, nrow(x)), x)
    attr(x2, "scaled:center") <- attr(x, "scaled:center")
    attr(x2, "scaled:scale") <- attr(x, "scaled:scale")
    attr(x2, "constant_columns") <- attr(x, "constant_columns")
    attr(x2, "intercept") <- TRUE
    x2
}

remove_intercept_column <- function(x) {
    x2 <- x[, -1, drop = FALSE]
    attr(x2, "scaled:center") <- attr(x, "scaled:center")
    attr(x2, "scaled:scale") <- attr(x, "scaled:scale")
    attr(x2, "constant_columns") <- attr(x, "constant_columns")
    x2
}

#'
#' @importFrom stats sd
remove_constant_columns <- function(x, epsilon = .Machine$double.eps) {
    stddv <- apply(x, 2, sd)
    mask <- which(stddv < epsilon)
    if (length(mask) == 0) {
        return(x)
    }
    x <- x[, -mask, drop = FALSE]
    attr(x, "constant_columns") <- mask
    x
}

add_constant_columns <- function(x, columns) {
    if (length(columns) == 0) {
        x
    } else if (is.null(dim(x))) {
        x2 <- rep(0, length(x) + length(columns))
        x2[-columns] <- x
        x2
    } else {
        x2 <- matrix(0, nrow(x), ncol(x) + length(columns))
        x2[, -columns] <- x
        x2
    }
}

unscale_alpha <- function(alpha, x_center, x_scale, y_center = NULL, y_scale = NULL) {
    if (is.null(y_center) && is.null(y_scale)) {
        if (!hasattr(x_scale, "scaled:scale") && !hasattr(x_center, "scaled:scale")) {
            stop("X and Y must have the scaled attributes")
        }
        y_center <- attr(x_scale, "scaled:center")
        y_scale <- attr(x_scale, "scaled:scale")
        x_scale <- attr(x_center, "scaled:scale")
        x_center <- attr(x_center, "scaled:center")
    }
    if (length(alpha) == length(x_center)) {
        alpha <- c(0, alpha)
    }
    alpha[1] <- (alpha[1] - sum(alpha[-1] * x_center / x_scale)) * y_scale + y_center
    alpha[-1] <- alpha[-1] / x_scale * y_scale
    alpha
}

#' Robust Scale
#' A variant of 'scale' that is based on median and mad (instead of mean and sd).
#' It can handle zero variance without producing nan:s.
#'
#' @param x the vector/matrix to normalise
#' @param th threshold for the scale being zero
#'
#' @return scaled_x (with attributes "scaled:center" and "scaled:scale")
#' @export
#'
#' @importFrom stats median
#' @importFrom stats mad
#'
scale_robust <- function(x, th = .Machine$double.eps) {
    if (is.null(dim(x))) {
        # Vector
        center <- median(x)
        scale <- mad(x, center)
        if (scale < th) {
            scale <- 1
        }
        x <- (x - center) / scale
    } else {
        # Matrix
        center <- apply(x, 2, median)
        x <- sweep(x, 2, center)
        scale <- apply(x, 2, mad, 0)
        scale[scale < th] <- 1
        x <- sweep(x, 2, scale, `/`)
    }
    attr(x, "scaled:center") <- c(center)
    attr(x, "scaled:scale") <- c(scale)
    x
}

#' Unscale a scaled matrix / vector
#'
#' @param x the matrix / vector to unscale
#' @param scaled optional object with "scaled:..." attributes
#'
#' @return x unscaled
#'
unscale <- function(x, scaled = NULL) {
    if (is.null(scaled)) {
        scaled <- x
    }
    center <- attr(scaled, "scaled:center")
    scale <- attr(scaled, "scaled:scale")
    if (is.null(dim(x))) { # Vector
        x <- x * scale + center
    } else { # Matrix
        x <- sweep(x, 2, scale, `*`)
        x <- sweep(x, 2, center, `+`)
    }
    attr(x, "scaled:center") <- NULL
    attr(x, "scaled:scale") <- NULL
    x
}


#' A variant of `scale` that only adds the attributes
#'
#' @param x the vector to (not) scale
#'
#' @return x (with attributes "scaled:center" and "scaled:scale")
#'
scale_identity <- function(x) {
    attr(x, "scaled:center") <- 0
    attr(x, "scaled:scale") <- 1
    x
}

scale_same <- function(x, center = NULL, scale = NULL, constant_columns = NULL) {
    if (is.null(center)) {
        return(x)
    }
    if (is.null(scale) && is.null(constant_columns) && hasattr(center, "scaled:scale")) {
        constant_columns <- attr(center, "constant_columns")
        scale <- attr(center, "scaled:scale")
        center <- attr(center, "scaled:center")
    }
    if (is.null(dim(x))) {
        if (!is.null(constant_columns)) {
            x <- x[-constant_columns]
        }
        x <- (x - center) / scale
    } else {
        if (!is.null(constant_columns)) {
            x <- x[, -constant_columns, drop = FALSE]
        }
        x <- sweep(sweep(x, 2, center), 2, scale, `/`)
    }
    attr(x, "scaled:scale") <- scale
    attr(x, "scaled:center") <- center
    attr(x, "constant_columns") <- constant_columns
    x
}

#' Calculate the PCA rotation matrix
#' The implementation is based on stats::prcomp.
#' Assumes the data has already been centered and scaled (if that is desired).
#'
#' @param X the matrix to reduce
#' @param dimensions the number of dimensions after PCA
#' @param tolerance remove components with variance less than the tolerance
#'
#' @return pca rotation matrix
#' @export
#'
simple_pca <- function(X, dimensions, tolerance = 1e-10) {
    # PCA to a desired number of dimensions
    dimensions <- min(dimensions, ncol(X))
    s <- svd(X, nu = 0, nv = dimensions)
    dimensions <- min(dimensions, length(s$d))
    pca_rotation <- s$v
    # Remove columns with too little variance
    rank <- sum(s$d[1:dimensions] > (s$d[[1]] * tolerance), na.rm = TRUE)
    if (rank < ncol(pca_rotation)) {
        pca_rotation <- pca_rotation[, 1:rank, drop = FALSE]
    }
    # Return the rotation matrix (only)
    pca_rotation
}