
library(datasets)

DATA_DIR <- "experiments/data"


#' Get EMNIST data
#'
#' @param digit the digit for the classification
#' @param n number of images (default = -1 == all images)
#' @param d number of dimensions (default = -1 == all dimensions)
#' @param th discard dimensions with variance less than th
#' @param balance should the number of samples in each class be balanced
#' @param index item to override the digit from (default = -1 == use digit instead)
#'
#' @return list(X = data, Y = prediction, R = real_class, mask = selected image indices)
#'
data_emnist <- function(digit = 2, n = -1, d = -1, th = -1, balance = TRUE, index = -1) {
    emnist <- readRDS(file.path(DATA_DIR, "emnist.rds"))
    X <- emnist$image
    R <- c(emnist$label)
    Y <- readRDS(file.path(DATA_DIR, "emnist_preds.rds"))
    if (index > 0) {
        digit <- R[index]
    }
    mask <- NULL
    if (balance) {
        mask <- which(R == digit)
        if (n > 0 && length(mask) > n / 2) {
            mask <- sample(mask, n / 2)
        }
        mask <- c(mask, sample(which(R != digit), length(mask)))
        mask <- sample(mask)
        X <- X[mask, , drop = FALSE]
        Y <- Y[mask, digit + 1]
        R <- R[mask]
    } else if (n > 0 && length(R) > n) {
        mask <- sample.int(length(R), n)
        X <- X[mask, , drop = FALSE]
        Y <- Y[mask, digit + 1]
        R <- R[mask]
    } else {
        Y <- Y[, digit + 1]
    }
    colnames(X) <- sprintf("Pixel[%02d,%02d]", rep(1:28, 28), rep(1:28, each = 28))
    if (th >= 0) {
        X <- X[, apply(X, 2, var, na.rm = TRUE) > th, drop = FALSE]
    }
    if (d > 0 && ncol(X) > d) {
        X <- X[, seq(1, ncol(X), length.out = d)]
    }
    list(X = X, Y = Y, R = R, mask = mask, name = paste("emnist", digit))
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
#'
#' @return list(X = data matrix, Y = prediction, R = real_class)
#'
data_jets <- function(img = FALSE, n = -1) {
    data <- readRDS(file.path(DATA_DIR, if (img) "jets_img.rds" else "jets.rds"))
    if (n > 0) {
        mask <- sample(c(sample(which(data$R == 1), n / 2), sample(which(data$R == 0), n / 2)))
        data$R <- data$R[mask]
        data$Y <- data$Y[mask]
        data$X <- data$X[mask, ]
    }
    data$name <- if (img) "jet images" else "jets"
    data
}

#' Get imdb review data
#'
#' @param n number of reviews
#' @param set test/train dataset
#' @param model classifier (svm, rf, elm)
#'
#' @return list(X = data matrix, Y = prediction, R = real_class)
#'
data_imdb <- function(n = -1, set = "test", model = "svm") {
    tmp <- readRDS(file.path(DATA_DIR, paste0("aclimdb_data_", set, ".rds")))
    X <- as.matrix(tmp$data)
    R <- as.numeric(tmp$class == "pos")
    tmp <- readRDS(file.path(DATA_DIR, paste0("aclimdb_pred_", set, ".rds")))
    Y <- as.numeric(tmp[[model]])
    if (n > 0 && n < length(Y)) {
        mask <- sample.int(length(Y), n)
        X <- X[mask, ]
        Y <- Y[mask]
        R <- R[mask]
    }
    list(X = X, Y = Y, R = R, name = "imdb")
}

#' Get mtcars data
#'
#' @param model type of model to train (rf, lm, or svm)
#'
#' @return list(X = data matrix, Y = predictions, R = real_value)
#'
data_mtcars <- function(model = "rf") {
    if (model == "rf") {
        mod <- randomForest::randomForest(mpg ~ ., mtcars)
    } else if (model == "lm") {
        mod <- lm(mpg ~ ., mtcars)
    } else if (model == "svm") {
        mod <- e1071::svm(mpg ~ ., mtcars)
    } else {
        stop("Unknown model type")
    }
    list(
        X = as.matrix(mtcars[-1]),
        Y = predict(mod, mtcars),
        R = mtcars$mpg,
        model = mod,
        name = "mtcars"
    )
}