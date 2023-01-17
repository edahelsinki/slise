
library(datasets)
source("experiments/data/retrieve_aclimdb.R")

DATA_DIR <- "experiments/data"


#' Get EMNIST data
#'
#' @param digit the digit for the classification
#' @param n number of images (default = -1 == all images)
#' @param d number of dimensions (default = -1 == all dimensions)
#' @param th discard dimensions with variance less than th
#' @param balance should the number of samples in each class be balanced
#' @param index item to override the digit from (default = -1 == use digit instead)
#' @param pred_fn return the prediction function
#'
#' @return list(X = data, Y = prediction, R = real_class, mask = selected image indices)
#'
data_emnist <- function(digit = 2, n = -1, d = -1, th = -1, balance = TRUE, index = -1, pred_fn = FALSE, ...) {
    emnist <- readRDS(file.path(DATA_DIR, "emnist.rds"))
    X <- emnist$image
    R <- c(emnist$label)
    Y <- readRDS(file.path(DATA_DIR, "emnist_preds.rds"))
    if (index > 0) {
        digit <- R[index]
    }
    # More sensible probabilities for a binary classifier
    Y <- Y[, digit + 1] / (Y[, digit + 1] + apply(Y[, -digit - 1, drop = FALSE], 1, max))
    mask <- NULL
    if (balance) {
        mask <- which(R == digit)
        if (n > 0 && length(mask) > n / 2) {
            mask <- sample(mask, n / 2)
        }
        mask <- c(mask, sample(which(R != digit), length(mask)))
        mask <- sample(mask)
        X <- X[mask, , drop = FALSE]
        Y <- Y[mask]
        R <- R[mask]
    } else if (n > 0 && length(R) > n) {
        mask <- sample.int(length(R), n)
        X <- X[mask, , drop = FALSE]
        Y <- Y[mask]
        R <- R[mask]
    }
    colnames(X) <- sprintf("Pixel[%02d,%02d]", rep(1:28, 28), rep(1:28, each = 28))
    if (th >= 0) {
        X <- X[, apply(X, 2, var, na.rm = TRUE) > th, drop = FALSE]
    }
    if (d > 0 && ncol(X) > d) {
        X <- X[, seq(1, ncol(X), length.out = d)]
    }
    if (pred_fn) {
        model <- keras::load_model_hdf5(file.path(DATA_DIR, "emnist_model.hdf5"))
        pred_fn <- function(x) {
            dim(x) <- c(length(x) / 784, 784)
            y <- predict(model, x)
            dim(y) <- c(dim(x)[1], 10)
            y <- y[, digit + 1] / (y[, digit + 1] + apply(y[, -digit - 1, drop = FALSE], 1, max))
            cbind(1 - y, y)
        }
    } else {
        pred_fn <- NULL
    }
    list(
        X = X,
        Y = Y,
        R = R,
        mask = mask,
        name = paste("emnist", digit),
        pred_fn = pred_fn,
        epsilon = 0.6,
        lambda1 = 2,
        lambda2 = 4,
        class = TRUE
    )
}


#' Get EMNIST internal data
#'
#' @param index index of item used as a start for the activation maximisation
#' @param data output from data_emnist()
#'
#' @return list(X = data, X2 = internal_states, Y = prediction, R = real_class, mask = selected image indices, nodes = activation maximisation, selected = the item used for the AM)
#'
data_emnist_internal <- function(index = 2400, data = data_emnist(index = index)) {
    internal <- readRDS(file.path(DATA_DIR, "emnist_internal.rds"))
    if (is.null(data$mask)) {
        data$X2 <- internal$internal
        data$selected <- index
    } else {
        data$X2 <- internal$internal[data$mask, , drop = FALSE]
        data$selected <- which(data$mask == index)
    }
    sel <- which(internal$selected == index)
    if (length(sel) != 1) {
        stop(paste("Index ", index, "has no prepared activation maximisation"))
    }
    data$nodes <- lapply(internal$nodes, function(n) n[sel, ])
    data$name <- "emnist internal"
    data
}

#' Get Physics data
#'
#' @param n number of jets
#' @param img image or tabular (default) format
#' @param scale robustly scale X (if not img)
#' @param pred_fn return the prediction function
#'
#' @return list(X = data matrix, Y = prediction, R = real_class)
#'
data_jets <- function(img = FALSE, n = -1, scale = TRUE, pred_fn = FALSE) {
    data <- readRDS(file.path(DATA_DIR, if (img) "jets_img.rds" else "jets.rds"))
    if (scale) {
        if (img) {
            scale <- median(apply(data$X, 1, max))
            data$X <- data$X / scale
            attr(data$X, "scaled:center") <- 0.0
            attr(data$X, "scaled:scale") <- scale
        } else {
            data$X <- scale_robust(data$X)
        }
    }
    if (n > 0) {
        mask <- sample(c(sample(which(data$R == 1), n / 2), sample(which(data$R == 0), n / 2)))
        data$R <- data$R[mask]
        data$Y <- data$Y[mask]
        center <- attr(data$X, "scaled:center")
        scale <- attr(data$X, "scaled:scale")
        data$X <- data$X[mask, ]
        attr(data$X, "scaled:center") <- center
        attr(data$X, "scaled:scale") <- scale
    }
    data$R <- factor(c("Gluon", "Quark"))[data$R + 1]
    data$class <- TRUE
    if (img) {
        data$name <- "jet images"
        data$model <- "cnn"
        data$epsilon <- 0.6
        data$lambda1 <- 20.0
        data$lambda2 <- 40.0
    } else {
        data$epsilon <- 0.25
        data$lambda1 <- 20.0
        data$lambda2 <- 0.0
        data$name <- "jets"
        data$model <- "nn"
    }
    if (pred_fn) {
        model <- keras::load_model_hdf5(file.path(DATA_DIR, if (img) "jets_img.hdf5" else "jets.hdf5"))
        if (scale) {
            data$pred_fn <- function(x, ...) {
                p <- predict(model, unscale(x, data$X), ...)
                cbind(1 - p, p)
            }
        } else {
            data$pred_fn <- function(...) {
                p <- predict(model, ...)
                cbind(1 - p, p)
            }
        }
    } else {
        data$pred_fn <- NULL
    }
    data
}

#' Get imdb review data
#'
#' @param n number of reviews
#' @param set test/train dataset
#' @param model classifier (svm, rf, elm)
#' @param pred_fn return the prediction function
#'
#' @return list(X = data matrix, Y = prediction, R = real_class)
#'
data_imdb <- function(n = -1, set = "test", model = "svm", pred_fn = FALSE, ...) {
    tmp <- aclimdb_get_data(set)
    X <- as.matrix(tmp$data)
    R <- as.numeric(tmp$class == "pos")
    Y <- c(tmp[[paste0("prob_", model)]])
    if (n > 0 && n < length(Y)) {
        mask <- sample.int(length(Y), n)
        X <- X[mask, ]
        Y <- Y[mask]
        R <- R[mask]
    }
    list(
        X = X,
        Y = Y,
        R = R,
        name = "imdb",
        model = model,
        pred_fn = if (pred_fn) aclimdb_get_model(model) else NULL,
        epsilon = 0.2,
        lambda1 = 0.01,
        lambda2 = 0.0,
        class = TRUE
    )
}

#' Get mtcars data (This is a smaller version of the Auto MPG dataset)
#'
#' @param model type of model to train (rf, lm, or svm)
#' @param scale robustly scale X and Y
#'
#' @return list(X = data matrix, Y = predictions, R = real_value)
#'
data_mtcars <- function(model = "rf", scale = FALSE, ...) {
    X <- as.matrix(mtcars[-1])
    Y <- mtcars$mpg
    if (scale) {
        X <- scale_robust(X)
        Y <- scale_robust(Y)
    }
    if (model == "rf") {
        mod <- randomForest::randomForest(X, Y)
    } else if (model == "lm") {
        mod <- lm(Y ~ X)
    } else if (model == "svm") {
        mod <- e1071::svm(Y ~ X)
    } else {
        stop("Unknown model type")
    }
    list(
        X = X,
        Y = predict(mod, X),
        R = mtcars$mpg,
        model = model,
        model_obj = mod,
        pred_fn = function(...) predict(mod, ...),
        name = "mtcars",
        epsilon = 0.2,
        lambda1 = 0.1,
        lambda2 = 0.0,
        class = FALSE
    )
}