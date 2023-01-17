# This script contains wrapper functions for explanations with LIME, SHAP, and SLISE

source("experiments/explanations/utils.R")

require(keras)
library(reticulate)
library(tensorflow)


# This version uses superpixels from LIME but with SLISE instead of LASSO
limeslise_emnist <- function(X,
                             Y,
                             index,
                             predict_fn,
                             ...,
                             samples = 10000,
                             compactness = 0.5,
                             removed = 0.25,
                             similarity = 0.2,
                             replacement = 0.5,
                             epsilon = 0.6,
                             lambda1 = 5,
                             lambda2 = 10) {
    x <- c(X[index, ])
    y <- Y[index]
    lime <- reticulate::import("lime")
    segmenter <- lime$wrappers$scikit_image$SegmentationAlgorithm("slic", compactness = compactness, convert2lab = FALSE, start_label = as.integer(1))
    size <- sqrt(ncol(X))
    segments <- c(segmenter(matrix(x, size, size)))
    segmenter <- NULL
    max_seg <- max(segments)
    dimred <- function(hx) {
        diff <- abs(hx - x)
        vapply(1:max_seg, function(i) mean(diff[segments == i]) < similarity, numeric(1))
    }
    dimexp <- function(lx) {
        ifelse(segments %in% which(as.logical(lx)), x, x * 0 + replacement)
    }
    X <- matrix(as.numeric(runif(samples * max_seg) > removed), samples, max_seg)
    X <- rbind(rep(1, ncol(X)), X)
    HX <- t(apply(X, 1, dimexp))
    Y <- predict_fn(HX)
    weight <- exp(-apply(X, 1, cosine_distance, X[index, ]))
    weight <- weight * length(Y) / sum(weight) # uniformly scale so that the same lambdas can be used
    expl <- slise.explain(X, Y, epsilon, 1, lambda1 = lambda1, lambda2 = lambda2, logit = TRUE, weight = weight)
    nh <- list(X = HX[expl$subset, ], Y = Y[expl$subset])
    approx_fn <- function(X) {
        if (length(X) == length(x)) {
            predict(expl, dimred(X))
        } else {
            predict(expl, t(apply(X, 1, dimred)))
        }
    }
    list(
        coefficients = NULL,
        impact = expl$alpha[-1][segments],
        intercept = expl$alpha[[1]],
        approx_fn = approx_fn,
        neighbourhood = nh,
        name = "LIME-SLISE"
    )
}

# Pixelwise KernelSHAP with various options for "off" pixels
shap_emnist <- function(X,
                        Y,
                        index,
                        predict_fn,
                        ...,
                        deletion = "invert",
                        samples = 10000,
                        similarity = 0.2) {
    shap <- reticulate::import("shap")
    nhX <- NULL
    nhY <- NULL
    pred <- function(x) {
        out <- predict_fn(x)
        nhX <<- rbind(nhX, x)
        nhY <<- c(nhY, out)
        if (is.null(dim(out))) {
            dim(out) <- c(length(out), 1)
        }
        out
    }
    py_pred <- reticulate::py_func(pred)
    item <- unname(X[index, , drop = FALSE])
    if (deletion == "invert") {
        explainer <- shap$KernelExplainer(py_pred, 1 - item, "logit")
    } else if (deletion == "background") {
        explainer <- shap$KernelExplainer(py_pred, 0 * item, "logit")
    } else if (deletion == "gray" || deletion == "grey") {
        explainer <- shap$KernelExplainer(py_pred, 0 * item + 0.5, "logit")
    } else if (deletion == "sample") {
        # Manually do the logit-link function
        py_pred <- reticulate::py_func(function(x) {
            out <- pred(x)
            out2 <- limited_logit(out)
            dim(out2) <- dim(out)
            out2
        })
        # Subsample the data to increase speed
        Xsamp <- unname(X[seq(1, nrow(X), 10), ])
        explainer <- shap$explainers$Sampling(py_pred, Xsamp)
    } else {
        stop("Unkown deletion method (must be one of invert, background, grey, or sample)")
    }
    shap_values <- explainer$shap_values(item, nsamples = as.integer(samples), l1_reg = "aic", silent = TRUE)
    if (length(shap_values) == 1) {
        shap_values <- shap_values[[1]]
    }
    hm <- shap_values[1, ]
    intercept <- c(explainer$expected_value)
    pred <- function(X) sigmoid(apply(X, 1, function(x) intercept + sum(hm[abs(x - item) < similarity])))
    list(
        coefficients = NULL,
        impact = hm,
        intercept = intercept,
        approx_fn = pred,
        neighbourhood = list(X = nhX, Y = nhY),
        name = sprintf("SHAP (%s)", deletion)
    )
}

# SHAP for tabular data
shap_tabular <- function(X, Y, index, predict_fn, class = FALSE, ..., samples = 10000, similarity = 0.2) {
    # Values are similar if their absolute differences are less than x%
    similarity <- apply(abs(sweep(data$X, 2, data$X[index, ])), 2, quantile, similarity) + 1e-8
    shap <- reticulate::import("shap")
    nhX <- NULL
    nhY <- NULL
    pred <- function(x) {
        dim(x) <- c(length(x) / ncol(X), ncol(X))
        colnames(x) <- colnames(X)
        out <- predict_fn(x)
        if (class) {
            out <- limited_logit(out)
        }
        if (is.null(dim(out))) {
            dim(out) <- c(length(out), 1)
        }
        nhX <<- rbind(nhX, x)
        nhY <<- rbind(nhY, out)
        out
    }
    py_pred <- reticulate::py_func(pred)
    item <- unname(X[index, , drop = FALSE])
    explainer <- shap$explainers$Sampling(py_pred, X)
    shap_values <- explainer$shap_values(item, nsamples = as.integer(samples), l1_reg = "aic", silent = TRUE)
    if (length(shap_values) == 1) {
        shap_values <- shap_values[[1]]
    }
    colnames(shap_values) <- colnames(X)
    hm <- shap_values[1, ]
    intercept <- c(explainer$expected_value)
    if (class) {
        pred <- function(X) sigmoid(apply(X, 1, function(x) intercept + sum(hm[abs(x - item) < similarity])))
    } else {
        pred <- function(X) apply(X, 1, function(x) intercept + sum(hm[abs(x - item) < similarity]))
    }
    list(
        coefficients = NULL, # hm,
        impact = hm,
        intercept = intercept,
        approx_fn = pred,
        neighbourhood = list(X = nhX, Y = nhY),
        name = "SHAP"
    )
}

# Give a random vector as a linear approximation for EMNIST
rndexpl_emnist <- function(X, Y, index, predict_fn = NULL, ..., mean = 0, stddv = 0.25) {
    hm <- rnorm(ncol(X) + 1, mean, stddv)
    nh <- list(X = X, Y = Y)
    approx_fn <- function(X) sigmoid(X %*% hm[-1] + hm[1])
    list(
        coefficients = hm[-1],
        impact = NULL,
        intercept = hm[[1]],
        approx_fn = approx_fn,
        neighbourhood = nh,
        name = "Random"
    )
}

# Give a random vector as a linear approximation for tabular data
rndexpl_tabular <- function(X, Y, index, predict_fn = NULL, class = FALSE, ..., mean = 0, stddv = 1) {
    hm <- rnorm(ncol(X) + 1, mean, stddv)
    if (class) {
        approx_fn <- function(X) sigmoid(X %*% hm[-1] + hm[1])
    } else {
        approx_fn <- function(X) (X %*% hm[-1] + hm[1])
    }
    list(
        coefficients = hm[-1],
        impact = X[index, ] * hm[-1],
        intercept = hm[[1]],
        approx_fn = approx_fn,
        neighbourhood = list(X = X, Y = Y),
        name = "Random"
    )
}

# Give a global linear model as the approximation for EMNIST
global_emnist <- function(X, Y, index, predict_fn = NULL, ...) {
    Y <- limited_logit(Y)
    expl <- .lm.fit(add_intercept_column(X), Y)
    nh <- list(X = X, Y = Y)
    approx_fn <- function(X) sigmoid(add_intercept_column(X) %*% expl$coefficients)
    list(
        coefficients = expl$coefficients[-1],
        impact = NULL,
        intercept = expl$coefficients[[1]],
        approx_fn = approx_fn,
        neighbourhood = nh,
        name = "Global"
    )
}

# Give a global linear model as the approximation for tabular data
global_tabular <- function(X, Y, index, predict_fn = NULL, class = FALSE, ...) {
    if (class) {
        Y <- limited_logit(Y)
    }
    expl <- .lm.fit(add_intercept_column(X), Y)
    if (class) {
        approx_fn <- function(X) sigmoid(add_intercept_column(X) %*% expl$coefficients)
    } else {
        approx_fn <- function(X) add_intercept_column(X) %*% expl$coefficients
    }
    list(
        coefficients = expl$coefficients[-1],
        impact = X[index, ] * expl$coefficients[-1],
        intercept = expl$coefficients[[1]],
        approx_fn = approx_fn,
        neighbourhood = list(X = X, Y = Y),
        name = "Global"
    )
}

# SLISE explanation for EMNIST, but with output the same as the other explainers
slise_emnist <- function(X,
                         Y,
                         index,
                         predict_fn = NULL,
                         ...,
                         epsilon = 0.5,
                         lambda1 = 2,
                         lambda2 = 2) {
    expl <- slise.explain(X, Y, epsilon, index, lambda1 = lambda2, lambda2 = lambda2, logit = TRUE)
    nh <- list(X = X[expl$subset, ], Y = Y[expl$subset])
    approx_fn <- function(X) predict(expl, X)
    list(
        coefficients = expl$alpha[-1],
        impact = NULL,
        intercept = expl$alpha[[1]],
        approx_fn = approx_fn,
        neighbourhood = nh,
        name = "SLISE"
    )
}

# SLISE explanation for tabular data, but with output the same as the other explainers
slise_tabular <- function(X,
                          Y,
                          index,
                          predict_fn = NULL,
                          class = FALSE,
                          ...,
                          epsilon = 0.15,
                          lambda1 = 0.1,
                          lambda2 = 0.0) {
    expl <- slise.explain(X, Y, epsilon, index, lambda1 = lambda1, lambda2 = lambda2, logit = class)
    list(
        coefficients = expl$alpha[-1],
        impact = X[index, ] * expl$alpha[-1],
        intercept = expl$alpha[[1]],
        approx_fn = function(X) predict(expl, X),
        neighbourhood = list(X = X[expl$subset, ], Y = Y[expl$subset]),
        name = "SLISE"
    )
}

# SLISE explanation with distance weights, but with output the same as the other explainers
distslise_emnist <- function(X,
                             Y,
                             index,
                             predict_fn = NULL,
                             ...,
                             epsilon = 0.5,
                             lambda1 = 2,
                             lambda2 = 2,
                             distance = cosine_distance,
                             divide_by_median_dist = FALSE) {
    dist <- apply(X, 1, distance, X[index, ])
    if (divide_by_median_dist) {
        dist <- dist / median(dist)
    }
    weight <- exp(-dist)
    weight <- weight * length(Y) / sum(weight) # uniformly scale so that the same lambdas can be used
    expl <- slise.explain(X, Y, epsilon, index, lambda1 = lambda1, lambda2 = lambda2, weight = weight, logit = TRUE)
    nh <- list(X = X[expl$subset, ], Y = Y[expl$subset])
    approx_fn <- function(X) predict(expl, X)
    list(
        coefficients = expl$alpha[-1],
        impact = NULL,
        intercept = expl$alpha[[1]],
        approx_fn = approx_fn,
        neighbourhood = nh,
        name = "SLISE (weighted)"
    )
}

# LIME forexplaining EMNIST, with different superpixel sizes
lime_emnist <- function(X,
                        Y,
                        index,
                        predict_fn,
                        ...,
                        segmentation = "original",
                        samples = 10000,
                        verbose = FALSE,
                        similarity = 0.2,
                        replacement = 0.5) {
    lime <- reticulate::import("lime")
    explainer <- lime$lime_image$LimeImageExplainer(verbose = verbose)
    size <- sqrt(ncol(X))
    py_img <- reticulate::r_to_py(matrix(X[index, ], size, size))
    nhX <- NULL
    nhY <- NULL
    py_pred <- reticulate::py_func(function(x) {
        x <- x[, , , 1]
        dim(x) <- c(dim(x)[1], length(x) / dim(x)[1])
        out <- predict_fn(x)
        nhX <<- rbind(nhX, x)
        nhY <<- c(nhY, out)
        cbind(1 - out, out)
    })
    if (segmentation == "small") {
        segmenter <- lime$wrappers$scikit_image$SegmentationAlgorithm("slic", compactness = 0.5, convert2lab = FALSE, start_label = as.integer(0))
    } else if (segmentation == "original") {
        # This is taken from the MNIST example in the LIME repository (august 2021):
        segmenter <- lime$wrappers$scikit_image$SegmentationAlgorithm("quickshift", kernel_size = 1, max_dist = 200, ratio = 0.2, start_label = as.integer(0))
    } else if (segmentation == "pixel") {
        segmenter <- lime$wrappers$scikit_image$SegmentationAlgorithm("quickshift", kernel_size = 1, max_dist = 0.1, ratio = 1, convert2lab = FALSE, start_label = as.integer(0))
    } else {
        stop("Uknown segmentation (must be one of original, small, or pixel)")
    }
    expl <- explainer$explain_instance(
        py_img,
        classifier_fn = py_pred,
        num_samples = as.integer(samples),
        batch_size = as.integer(1000),
        segmentation_fn = segmenter,
        hide_color = replacement
    )
    lime <<- expl
    intercept <- c(expl$intercept[[2]])
    local_exp <- c(expl$local_exp[[2]])
    segments <- c(expl$segments)
    img <- segments * 0
    for (e in local_exp) {
        img[segments == e[[1]]] <- e[[2]]
    }
    img <- c(img)
    # alpha <- c(intercept, sapply(local_exp, function(e) e[[2]]))
    size <- ncol(X)
    pred <- function(x) {
        dim(x) <- c(length(x) / size, size)
        apply(x, 1, function(x) {
            out <- intercept
            for (e in local_exp) {
                mask <- segments == e[[1]]
                if (mean(abs(X[index, mask] - x[mask])) < similarity) {
                    out <- out + e[[2]]
                }
            }
            out
        })
    }
    list(
        coefficients = NULL,
        impact = img,
        intercept = intercept,
        approx_fn = pred,
        neighbourhood = list(X = nhX, Y = nhY),
        name = sprintf("LIME (%s)", segmentation)
    )
}

# LIME explanation for tabular data
lime_tabular <- function(X, Y, index, predict_fn = NULL, class = FALSE, ..., discretise = FALSE, samples = 10000) {
    lime_tabular <- reticulate::import("lime.lime_tabular")
    explainer <- lime_tabular$LimeTabularExplainer(
        training_data = X,
        mode = ifelse(class, "classification", "regression"),
        feature_names = colnames(X),
        discretize_continuous = discretise
    )
    nhX <- NULL
    nhY <- NULL
    pred <- function(x) {
        if (length(dim(x)) < 2) {
            dim(x) <- c(1, length(x))
        }
        colnames(x) <- colnames(X)
        out <- predict_fn(x)
        if (is.null(dim(out))) {
            dim(out) <- c(length(out), 1)
        }
        nhX <<- rbind(nhX, x)
        nhY <<- rbind(nhY, out)
        if (class) {
            cbind(1 - out, out)
        } else {
            out
        }
    }
    py_pred <- reticulate::py_func(pred)
    item <- reticulate::r_to_py(X[index, , drop = FALSE], FALSE)[0]
    expl <- explainer$explain_instance(
        item,
        predict_fn = py_pred,
        num_samples = as.integer(samples)
    )
    intercept <- c(expl$intercept[[1]])
    coef <- rep(0, ncol(X))
    for (ic in expl$local_exp[[1]]) {
        coef[ic[[1]] + 1] <- ic[[2]]
    }
    if (!class) {
        coef <- -coef
    }
    if (discretise) {
        itemd <- explainer$discretizer$discretize(item)
        approx_fn <- function(X) {
            Xd <- explainer$discretizer$discretize(X)
            c(t(apply(Xd, 1, function(x) x == itemd)) %*% coef + intercept[1])
        }
    } else {
        approx_fn <- function(X) c((X %*% coef + intercept[1]))
    }
    list(
        coefficients = if (discretise) NULL else coef,
        impact = if (discretise) coef else coef * X[index, ],
        intercept = intercept,
        approx_fn = approx_fn,
        neighbourhood = list(X = nhX, Y = c(nhY)),
        name = ifelse(discretise, "LIME", "LIME (nd)")
    )
}



#' Create wrappers for the classifier so LIME can use them
#' This can be used instead of as_classifier
#'
#' @param model classifier
#' @param labels class names
#'
#' @return
#'
lime_model_wrapper <- function(model, labels) {
    if ("svm" %in% class(model)) {
        as_classifier(structure(list(svm = model), class = "svm_wrapper"), labels)
    } else if ("lr" %in% class(model)) {
        as_classifier(structure(list(model = model), class = "lr_wrapper"), labels)
    } else if ("keras.engine.training.Model" %in% class(model)) {
        as_classifier(structure(list(model = model, labels = labels), class = "keras_wrapper"), labels)
    } else {
        as_classifier(model, labels)
    }
}

predict.svm_wrapper <- function(model, newdata, ...) {
    p_svm <- predict(model$svm, newdata = newdata, probability = TRUE)
    p <- unname(attr(p_svm, "probabilities"))[, 1]
    cbind(1 - p, p)
}

predict.lr_wrapper <- function(model, newdata, ...) {
    p <- predict(model$model, newdata = newdata)
    p <- sigmoid(unname(p))
    cbind(1 - p, p)
}

predict.keras_wrapper <- function(model, newdata, type = "raw", ...) {
    res <- predict(model$model, as.matrix(newdata))
    if (type == "raw") {
        data.frame(Response = res[, 1])
    } else {
        colnames(res) <- model$labels
        as.data.frame(res, check.names = FALSE)
    }
}