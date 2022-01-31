# This script contains wrapper functions for explanations with LIME, SHAP, and SLISE

require(keras)
library(reticulate)
library(tensorflow)

source("experiments/explanations/utils.R")

# This version uses superpixels from LIME but with SLISE instead of LASSO
limeslise_emnist <- function(X,
                             Y,
                             index,
                             predict_fn,
                             epsilon = 0.5,
                             lambda = 5,
                             samples = 10000,
                             compactness = 0.5,
                             removed = 0.25,
                             similarity = 0.2,
                             ...) {
    x <- c(X[index, ])
    y <- Y[index]
    lime <- reticulate::import("lime")
    segmenter <- lime$wrappers$scikit_image$SegmentationAlgorithm("slic", compactness = compactness, convert2lab = FALSE, start_label = as.integer(1))
    segments <- c(segmenter(matrix(x, 28, 28)))
    segmenter <- NULL
    max_seg <- max(segments)
    dimred <- function(hx) {
        diff <- abs(hx - x)
        vapply(1:max_seg, function(i) mean(diff[segments == i]) < similarity, numeric(1))
    }
    dimexp <- function(lx) {
        ifelse(segments %in% which(as.logical(lx)), x, x * 0 + 0.5)
    }
    X <- matrix(as.numeric(runif(samples * max_seg) > removed), samples, max_seg)
    X <- rbind(rep(1, ncol(X)), X)
    HX <- t(apply(X, 1, dimexp))
    Y <- predict_fn(HX)
    weight <- exp(-apply(X, 1, cosine_distance, X[index, ]))
    weight <- weight * length(Y) / sum(weight) # uniformly scale so that the same lambdas can be used
    expl <- slise.explain(X, Y, epsilon, 1, lambda1 = lambda, lambda2 = lambda, logit = TRUE, weight = weight)
    nh <- list(X = HX[expl$subset, ], Y = Y[expl$subset])
    approx_fn <- function(X) {
        if (length(X) == length(x)) {
            predict(expl, dimred(X))
        } else {
            predict(expl, t(apply(X, 1, dimred)))
        }
    }
    coef <- expl$alpha[-1][segments]
    list(heatmap = coef, impact = coef, alpha = expl$alpha, approx_fn = approx_fn, neighbourhood = nh, name = "LIME-SLISE")
}

# Pixelwise KernelSHAP with various options for "off" pixels
shap_emnist <- function(X,
                        Y,
                        index,
                        predict_fn,
                        deletion = "invert",
                        samples = 10000,
                        similarity = 0.2,
                        ...) {
    shap <- reticulate::import("shap")
    shap2 <- reticulate::import("shap", convert = FALSE)
    nhX <- NULL
    nhY <- NULL
    pred <- function(x) {
        out <- predict_fn(x)
        nhX <<- rbind(nhX, x)
        nhY <<- c(nhY, out)
        out
    }
    py_pred <- reticulate::py_func(pred)
    item <- unname(X[index, , drop = FALSE])
    if (deletion == "invert") {
        explainer <- shap$KernelExplainer(py_pred, 1 - item, "logit")
    } else if (deletion == "background") {
        explainer <- shap$KernelExplainer(py_pred, 0 * item, "logit")
    } else if (deletion == "gray") {
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
        stop("Unkown deletion method (must be one of invert, background, gray, or sample)")
    }
    shap_values <- explainer$shap_values(item, nsamples = as.integer(samples), l1_reg = "aic")
    if (length(shap_values) == 1) {
        shap_values <- shap_values[[1]]
    }
    hm <- shap_values[1, ]
    intercept <- c(explainer$expected_value)
    pred <- function(X) sigmoid(apply(X, 1, function(x) intercept + sum(hm[abs(x - item) < similarity])))
    list(heatmap = hm, impact = hm, alpha = c(intercept, hm), approx_fn = pred, neighbourhood = list(X = nhX, Y = nhY), name = sprintf("SHAP (%s)", deletion))
}

# Give a random vector as a linear approximation
rndexpl_emnist <- function(X,
                           Y,
                           index,
                           predict_fn = NULL,
                           mean = 0,
                           stddv = 0.25,
                           ...) {
    hm <- rnorm(ncol(X) + 1, mean, stddv)
    nh <- list(X = X, Y = Y)
    approx_fn <- function(X) sigmoid(X %*% hm[-1] + hm[1])
    list(heatmap = hm[-1], impact = hm[-1], alpha = hm, approx_fn = approx_fn, neighbourhood = nh, name = "Random")
}

# SLISE explanation, but with output the same as the other explainers
slise_emnist <- function(X,
                         Y,
                         index,
                         predict_fn = NULL,
                         ...,
                         epsilon = 0.5,
                         lambda = 2) {
    expl <- slise.explain(X, Y, epsilon, index, lambda1 = lambda, lambda2 = lambda, logit = TRUE)
    nh <- list(X = X[expl$subset, ], Y = Y[expl$subset])
    approx_fn <- function(X) predict(expl, X)
    coef <- expl$alpha[-1]
    list(heatmap = coef, impact = coef * (X[index, ] - 0.5), alpha = expl$alpha, approx_fn = approx_fn, neighbourhood = nh, name = "SLISE")
}

# SLISE explanation with distance weights, but with output the same as the other explainers
distslise_emnist <- function(X,
                             Y,
                             index,
                             predict_fn = NULL,
                             epsilon = 0.5,
                             lambda = 2,
                             distance = cosine_distance,
                             divide_by_median_dist = FALSE,
                             ...) {
    dist <- apply(X, 1, distance, X[index, ])
    if (divide_by_median_dist) {
        dist <- dist / median(dist)
    }
    weight <- exp(-dist)
    weight <- weight * length(Y) / sum(weight) # uniformly scale so that the same lambdas can be used
    expl <- slise.explain(X, Y, epsilon, index, lambda1 = lambda, lambda2 = lambda, weight = weight, logit = TRUE)
    nh <- list(X = X[expl$subset, ], Y = Y[expl$subset])
    approx_fn <- function(X) predict(expl, X)
    coef <- expl$alpha[-1]
    list(heatmap = coef, impact = coef * (X[index, ] - 0.5), alpha = expl$alpha, approx_fn = approx_fn, neighbourhood = nh, name = "SLISE (weighted)")
}

# LIME forexplaining EMNIST, with different superpixel sizes
lime_emnist <- function(X,
                        Y,
                        index,
                        predict_fn,
                        segmentation = "original",
                        samples = 10000,
                        verbose = FALSE,
                        similarity = 0.2,
                        ...) {
    lime <- reticulate::import("lime")
    explainer <- lime$lime_image$LimeImageExplainer(verbose = verbose)
    py_img <- reticulate::r_to_py(matrix(X[index, ], 28, 28))
    nhX <- NULL
    nhY <- NULL
    py_pred <- reticulate::py_func(function(x) {
        x <- x[, , , 1]
        dim(x) <- c(dim(x)[1], length(x) / dim(x)[1])
        out <- predict_fn(x)
        nhX <<- rbind(nhX, x)
        nhY <<- c(nhY, out)
        out
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
        hide_color = 0.5
    )
    intercept <- c(expl$intercept[[1]])
    local_exp <- c(expl$local_exp[[1]])
    segments <- c(expl$segments)
    img <- segments * 0
    for (e in local_exp) {
        img[segments == e[[1]]] <- e[[2]]
    }
    img <- c(img)
    alpha <- c(intercept, sapply(local_exp, function(e) e[[2]]))
    pred <- function(x) {
        dim(x) <- c(length(x) / 784, 784)
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
    list(heatmap = img, impact = img, alpha = alpha, approx_fn = pred, neighbourhood = list(X = nhX, Y = nhY), name = sprintf("LIME (%s)", segmentation))
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