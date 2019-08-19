# Helper methods for the experiments

require(MASS)
require(MTE)
require(robustHD)
require(sparseLTSEigen)
require(robustbase)
require(glmnet)
require(pense)
require(parallel)
library(slise)

DATA_DIR <- "experiments/data"

METHODS <- c("slise", "fastlts", "sparselts", "lasso", "ladlasso", "mm", "mmlasso")

#' Run all Robust Regression methods through a common interface
#'
#' @param X data matrix
#' @param Y response vector
#' @param method name of the method ("slise", "fastlts", "sparselts", "ols", "ladlasso", "mm")
#' @param epsilon error tolerance
#' @param ... other parameters to the methods
#' @param lambda sparsity coefficient
#' @param size lts subset size
#' @param scale should the data be scaled
#'
#' @return SLISE object
#'
regression <- function(X, Y = NULL, method = "slise", epsilon = 0.1, ..., lambda = 0, size = 0.5, scale = TRUE) {
    method <- tolower(method)
    if (method == "slise")
        return(slise.fit(X, Y, epsilon = epsilon, lambda = lambda, scale = scale, ...))
    data <- data_preprocess(X, Y, scale = scale, intercept = FALSE)
    if (method == "lasso") {
        res <- as.vector(predict(glmnet(data$X, data$Y, lambda = lambda), type="coefficients"))
    } else if (method == "ls" || method == "ols") {
        res <- lm(data$Y ~ data$X)$coefficients
    } else if (method == "resistant" || method == "lts") {
        res <- lqs(data$Y ~ data$X, stats::quantile = size)$coefficients
    } else if (method == "lts2" || method == "fastlts") {
        res <- ltsReg(data$X, data$Y, alpha = size, intercept = TRUE)$coefficients
    } else if (method == "lts3" || method == "sparselts") {
        res <- sparseLTS(data$X, data$Y, lambda, normalize = FALSE, intercept = TRUE, alpha = size, ncores = NA)$coefficients
    } else if (method == "huber") {
        res <- rlm(data$Y ~ data$X, init = "lts", psi = psi.huber, method = "M")$coefficients
    } else if (method == "bisquare" || method == "mm") {
        res <- rlm(data$Y ~ data$X, init = "lts", psi = psi.bisquare, method = "MM")$coefficients
    } else if (method == "lad" || method == "ladlasso") {
        res <- LAD(data$Y, data$X, TRUE)
        if (lambda > 0)
            res <- LADlasso(data$Y, data$X, beta.ini = res, lambda = lambda, adaptive = FALSE, intercept = TRUE)$beta
    } else if (method == "mmlasso") {
        cores <- detectCores(all.tests = FALSE, logical = TRUE)
        if (cores < 2) cores <- 4
        if (lambda == 0)
            res <- rlm(data$Y ~ data$X, init = "lts", psi = psi.bisquare, method = "MM")$coefficients
        else
            res <- as.vector(pensem(data$X, data$Y, alpha=1, lambda=lambda, ncores=cores)$coefficients)
    } else {
        stop("Unknown regression method")
    }
    create_slise(res, X, Y, data = data, epsilon = epsilon, lambda = lambda)
}

#' Convert method labels to presentable names
#'
#' @param method label 
#' @param latex is the output format latex
#'
#' @return name
#'
method_to_label <- function(method, latex = FALSE) {
    method <- as.character(method)
    if (latex) switch(method,
        "slise" = "\\slise",
        "fastlts" = "\\fastlts",
        "sparselts" = "\\sparselts",
        "ols" = "\\ols",
        "ladlasso" = "\\ladlasso",
        "mm" = "\\mmest",
        "mmlasso" = "\\mmlasso",
        "lasso" = "\\lasso",
        method)
    else switch(method,
        "slise" = "SLISE",
        "fastlts" = "fastLTS",
        "sparselts" = "sparseLTS",
        "ols" = "OLS",
        "ladlasso" = "LAD Lasso",
        "mm" = "MM Estimator",
        "mmlasso" = "MM Lasso",
        "lasso" = "Lasso",
        method)
}

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

#' Get data from the "London" dataset
#'
#' @param x data columns
#' @param y response column
#'
#' @return list(X, Y)
#'
data_pox <- function(x = "year", y = "all") {
    file <- file.path(DATA_DIR, "pox.csv")
    if (!file.exists(file)) {
        # http://vincentarelbundock.github.io/Rdatasets/doc/DAAG/poxetc.html
        download.file("http://vincentarelbundock.github.io/Rdatasets/csv/DAAG/poxetc.csv", file)
    }
    pox <- read.csv(file)
    pox$year <- pox$X + 1629
    mask <- is.finite(rowSums(as.matrix(pox)[, c(x, y)]))
    X <- as.matrix(pox)[mask, x, drop = FALSE]
    Y <- pox[[y]][mask]
    list(X = X, Y = Y)
}

# Get EMNIST data
# Params:
#   n:          number of images
#   d:          number of dimensions (default == -1 == all dimensions)
#   th:         discard dimensions with variance less than th
#   classifier: classifier ("digits" folowed by a number, or "high", or "even", followed by a number)
#   balance:    should the number of samples in each class be balanced
# Result
#   A data_container (X = data, Y = response, R = real_class)
data_emnist <- function(n = -1, d = -1, th = -1, classifier = "digits2", balance = TRUE) {
    emnist <- readRDS(file.path(DATA_DIR, "emnist.rds"))
    X <- emnist$image
    X2 <- NULL
    R <- emnist$label
    if (identical(substr(classifier, 1, nchar(classifier) - 1), "digits")) {
        data <- readRDS(file.path(DATA_DIR, "emnist_digits.rds"))
        number <- as.integer(substr(classifier, nchar(classifier), nchar(classifier)))
        Y <- data[, number + 1]
        # Adjusting for softmax
        Y2 <- apply(data[, - number - 1], 1, max)
        Y <- Y / (Y + Y2)
        # Balancing classes
        if (balance) {
            mask1 <- R == number
            mask2 <- which(!mask1)
            mask1 <- which(mask1)
            if (length(mask1) > length(mask2)) {
                mask1 <- sample(mask1, length(mask2))
            } else if (length(mask2) > length(mask1)) {
                mask2 <- sample(mask2, length(mask1))
            }
            mask <- sample(c(mask1, mask2))
            Y <- Y[mask]
            X <- X[mask,, drop = FALSE]
            R <- R[mask]
        }
    } else {
        Y <- readRDS(file.path(DATA_DIR, paste0("emnist_", classifier, ".rds")))
        mask <- seq_along(Y)
    }
    if (n > 0 && n < length(Y)) {
        mask2 <- sample.int(length(Y), n)
        X <- X[mask2,, drop = FALSE]
        Y <- Y[mask2]
        R <- R[mask2]
        mask <- mask[mask2]
    }
    colnames(X) <- paste("pixel", 1:ncol(X), sep = "")
    if (th >= 0)
        X <- X[, apply(X, 2, var, na.rm = TRUE) > th, drop = FALSE]
    if (d > 0 && d < ncol(X))
        X <- X[, sample.int(ncol(X), d), drop = FALSE]
    list(X = X, Y = Y, R = R, X2 = X2, M = mask)
}

#' Get Physics data
#'
#' @param n number of jets
#'
#' @return list(X, Y, R=real_class, model)
#'
data_jets <- function(n = -1) {
    data <- readRDS(file.path(DATA_DIR, "jets.rds"))
    if (n > 0) {
        mask <- sample(c(sample(which(data$R == 1), n / 2), sample(which(data$R == 0), n / 2)))
        data$R <- data$R[mask]
        data$Y <- data$Y[mask]
        data$X <- data$X[mask, ]
    }
    data
}

#' Get imdb review data
#'
#' @param n number of reviews
#' @param set test/train dataset
#' @param model classifier (svm, rf, lr, elm)
#'
#' @return list(X, Y, R=real_class, model)
#'
data_aclimdb <- function(n = -1, set = "test", model = "svm") {
    tmp <- readRDS(file.path(DATA_DIR, paste0("aclimdb_data_", set, ".rds")))
    X <- as.matrix(tmp$data)
    R <- as.numeric(tmp$class == "pos")
    Y <- as.numeric(tmp[[paste0("prob_", model)]])
    m <- readRDS(file.path(DATA_DIR, paste0("aclimdb_model_", model, ".rds")))
    class(m) <- c(model, class(m))
    if (n > 0 && n < length(Y)) {
        mask <- sample.int(length(Y), n)
        X <- X[mask, ]
        Y <- Y[mask]
        R <- R[mask]
    }
    list(X = X, Y = Y, R = R, model = m)
}

#' Get the census data
#'
#' @param n number of items
#' @param balance should the classes be balanced
#'
#' @return dataframe
#'
data_census <- function(n = -1, balance = FALSE) {
    df <- readRDS(file.path(DATA_DIR, "census.rds"))
    if (balance) {
        low <- which(df$income == "<=50K")
        high <- which(df$income == ">50K")
        if (length(low) > length(high))
            low <- sample(low, length(high))
        else if (length(low) < length(high))
            high <- sample(high, length(low))
        df <- df[sample(c(low, high)), ]
    }
    if (n > 0 && n < length(df)) {
        df <- df[sample.int(nrow(df), n), ]
    }
    df
}

#' Get a neural network model
#'
#' @param model the classifier ("emnist_digits", "jets")
#'
#' @return keras_model
#'
get_keras_model <- function(model = "emnist_digits") {
    library(keras)
    path <- paste0("experiments/data/", model, ".hdf5")
    stopifnot(file.exists(path))
    load_model_hdf5(path)
}
