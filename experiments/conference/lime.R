# This script contains helper functions for running LIME

require(abind)
require(magick)
require(keras)
require(lime)

source("experiments/utils.R")
source("experiments/data/retrieve_aclimdb.R")

#' Create wrappers for the classifier so LIME can use them
#' This can be used instead of as_classifier
#'
#' @param model classifier
#' @param labels class names
#'
#' @return
#'
lime_model_wrapper <- function(model, labels) {
    if ("svm" %in% class(model))
        as_classifier(structure(list(svm=model), class="svm_wrapper"), labels)
    else if ("lr" %in% class(model))
        as_classifier(structure(list(model=model), class="lr_wrapper"), labels)
    else if ("keras.engine.training.Model" %in% class(model))
        as_classifier(structure(list(model=model, labels=labels), class="keras_wrapper"), labels)
    else
        as_classifier(model, labels)
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

predict.keras_wrapper <- function(model, newdata, type="raw", ...) {
    res <- predict(model$model, as.matrix(newdata))
    if (type == "raw") {
        data.frame(Response = res[, 1])
    } else {
        colnames(res) <- model$labels
        as.data.frame(res, check.names = FALSE)
    }
}

#' Use LIME to explain a imdb review classification
#'
#' @param imdb data_imdb
#' @param index
#' @param n_features number of words to highlight
#' @param ... not used
#' @param show open a browser with the explanation (TRUE)
#'
#' @return lime_explanation_data_frame
#'
lime_explain_aclimdb <- function(imdb=NULL, index=-1, n_features = 10, ..., show=TRUE) {
    if (is.null(imdb))
        imdb <- data_aclimdb(10000)
    if (index < 1)
        index <- sample.int(length(imdb$Y), 1)
    review <- aclimdb_get_review(imdb, index)
    dict <- lapply(unique(review$tokens), function(l) which(colnames(imdb$X) == l))
    names(dict) <- unique(review$tokens)
    prep <- function(perms) {
        X <- do.call(rbind, lapply(perms, function(p) {
            x <- rep(0, ncol(imdb$X))
            for (s in stringr::str_split(p, " ")[[1]]) {
                v <- dict[[s]]
                if (!is.null(v) && length(v) > 0) x[[v]] <- x[[v]] + 1
            }
            x
        }))
        colnames(X) <- colnames(imdb$X)
        as.data.frame(X)
    }
    explainer <- lime::lime(review$text, lime_model_wrapper(imdb$model, c("negative", "positive")), preprocess=prep)
    expl <- lime::explain(review$text, explainer, n_labels = 1, n_features = n_features, single_explanation=TRUE)
    #expl$data <- sapply(expl$data, function(s) do.call(paste, as.list(eval(parse(text=s)))))
    if (show)
        print(plot_text_explanations(expl))
    invisible(expl)
}

#' Use LIME to explain a EMNIST classification
#'
#' @param x the number to explain
#' @param model the classifier
#' @param n_features the number of superpixels to highlighr
#' @param n_superpixels the number of superpixels to create
#' @param show plot the result (TRUE)
#' @param labels class names (0-9)
#'
#' @return lime_explanation_data_frame
#'
lime_explain_image <- function(x, model, n_features = 20, n_superpixels=-1, show=TRUE, labels=paste0("", 0:9)) {
    path <- sprintf("%s.png", tempfile())
    png(path)
    par(mar = rep(0, 4))
    image(array_reshape(-x, c(28, 28))[, 28:1], axes = FALSE, col = grey(seq(0, 1, length = 256)))
    dev.off()
    img_preprocess <- function(x) {
        arrays <- lapply(x, function(path) {
            img <- image_load(path, target_size = c(28, 28), grayscale = TRUE)
            img <- image_to_array(img)[1:28, 1:28, ]
            c(img)
        })
        1 - do.call(rbind, arrays) / 127.5
    }
    class(path) <- c("imagefile", class(path))
    explainer <- lime(path, as_classifier(model, labels), preprocess=img_preprocess)
    if (n_superpixels > 0)
        expl <- explain(path, explainer, n_labels=1, n_features=n_features, n_superpixels=n_superpixels, background="white")
    else
        expl <- explain(path, explainer, n_labels=1, n_features=n_features, background="white")
    if (show)
        plot(plot_image_explanation(expl, show_negative=n_features>1))
    invisible(expl)
}

#' Use LIME to explain a jet classification
#'
#' @param X data matrix
#' @param x jet to explain as a vector
#' @param model the classifier
#' @param n_features number of variables to use
#' @param plot plot the explanation (TRUE)
#'
#' @return lime_explanation_data_frame
#'
lime_explain_jets <- function(X, x, model, n_features=4, plot=TRUE) {
    labels <- c("Gluon", "Quark")
    explainer <- lime(as.data.frame(X), lime_model_wrapper(model, labels))
    lime <- explain(as.data.frame(x), explainer, n_labels = 1, n_features = n_features, dist_fun="euclidean")
    if (plot) {
        lime2 <- lime[order(abs(lime$feature_weight)), ]
        lime2$text <- paste0("Label: ", lime$label[[1]], "\nProbability: ", round(lime$label_prob[[1]], 2),
            "\nExplanation Fit: ", round(lime$model_r2[[1]], 2))
        lime2$feature_weight <- if (lime$label[[1]] == "Quark") lime2$feature_weight else -lime2$feature_weight
        plot(ggplot(lime2, aes(feature_desc, feature_weight, fill=sign(feature_weight))) +
            scale_x_discrete(limits = lime2$feature_desc[order(abs(lime2$feature_weight))]) + coord_flip() +
            facet_wrap(~text) + geom_col() + ggtitle("LIME Explanation for a Physics Jet") + theme_minimal() +
            xlab("Features") + ylab("Weight") + theme(legend.position = "bottom", strip.text = element_text(hjust=0, face="bold")) +
            scale_fill_gradient(low="#f1a340", high="#998ec3", guide=guide_legend(NULL), breaks=c(-1, 1), limits=c(-1, 1), labels=labels))
    }
    invisible(lime)
}
