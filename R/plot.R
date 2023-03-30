# This script contains functions for plotting, printing and showing SLISE regressions and explanations

SLISE_ORANGE <- "#fda411"
SLISE_PURPLE <- "#9887cb"
SLISE_WEAKPURPLE <- "#998ec344"
SLISE_DARKORANGE <- "#e66101"
SLISE_DARKPURPLE <- "#5e3c99"


#' Print the robust regression or explanation from slise
#'
#' @param x The slise object
#' @param num_vars Minimum number of variables to show without filtering (default: 10)
#' @param labels Name of y or class labels
#' @param ... Ignored additional parameters
#'
#' @return invisible(x)
#' @export
#'
#' @examples
#' X <- matrix(rnorm(30), 15, 2)
#' Y <- runif(15, 0, 1)
#' print(slise.fit(X, Y, epsilon = 0.1))
print.slise <- function(x, num_vars = 10, labels = NULL, ...) {
    check_package("stringr")
    slise <- x
    if (is.null(slise$x)) {
        cat("SLISE Regression:\n")
    } else {
        cat("SLISE Explanation:\n")
    }
    # Table of item, model and terms
    data <- rbind(
        `Explained Item` = c(slise$y, slise$x),
        `Linear Model` = slise$coefficients,
        `Prediction Term` = slise$terms,
        `Normalised Item` = c(slise$normalised_y, slise$normalised_x),
        `Normalised Model` = slise$normalised,
        `Normalised Term` = slise$normalised_terms
    )
    colnames(data) <- names(slise$coefficients)
    if (!is.null(slise$x)) {
        colnames(data)[1] <- "Response/Intercept"
    }
    ord <- order_coefficients(slise, FALSE, if (ncol(data) > num_vars) .Machine$double.eps else -1)
    if (slise$intercept) {
        ord <- c(1, ord + 1)
    }
    data <- data[, ord]
    print(data)
    # Other Values
    cat(sprintf("Subset size: %7.2f\n", mean(slise$subset)))
    cat(sprintf("Loss:        %7.2f\n", slise$value))
    if (is.null(slise$normalised_epsilon) || slise$normalised_epsilon == slise$epsilon) {
        cat(sprintf("Epsilon:     %7.2f\n", slise$epsilon))
    } else {
        cat(sprintf("Epsilon:     %7.2f   (Normalised: %7.2f)\n", slise$epsilon, slise$normalised_epsilon))
    }
    if (slise$lambda1 > 0) {
        cat(sprintf("Lambda1:     %7.2f\n", slise$lambda1))
    }
    if (slise$lambda2 > 0) {
        cat(sprintf("Lambda2:     %7.2f\n", slise$lambda2))
    }
    if (sparsity(slise$coefficients) < length(slise$coefficients)) {
        cat(sprintf(
            "Non-Zero:    %2d / %2d\n",
            sparsity(slise$coefficients),
            length(slise$coefficients)
        ))
    }
    cat(sprintf("Predicted: %s\n", string_prediction(slise$y, slise$logit, labels)))
    if (slise$logit) {
        if (length(labels) > 1) {
            cat(sprintf(
                "Class Balance: %.1f%% %s <> %.1f%% %s\n",
                mean(slise$Y[slise$subset] < 0) * 100,
                labels[1],
                mean(slise$Y[slise$subset] > 0) * 100,
                labels[2]
            ))
        } else {
            cat(sprintf(
                "Class Balance: %.1f%% <> %.1f%%\n",
                mean(slise$Y[slise$subset] < 0) * 100,
                mean(slise$Y[slise$subset] > 0) * 100
            ))
        }
    }
    invisible(slise)
}

order_coefficients <- function(slise, hightolow = FALSE, minimum = .Machine$double.eps) {
    alpha <- slise$coefficients
    if (!is.null(slise$normalised)) {
        alpha <- slise$normalised
    }
    if (slise$intercept) {
        alpha <- alpha[-1]
    }
    ord <- which(abs(alpha) > minimum)
    if (hightolow) {
        ord <- ord[order(alpha[ord], decreasing = TRUE)]
    } else {
        ord <- ord[order(abs(alpha[ord]), decreasing = TRUE)]
    }
    ord
}

string_prediction <- function(y, logit = FALSE, labels = NULL) {
    if (logit) {
        if (!is.null(labels)) {
            if (y < 0.0 && length(labels) > 1) {
                sprintf("%.1f%% %s", (1 - sigmoid(y)) * 100, labels[1])
            } else {
                sprintf("%.1f%% %s", sigmoid(y) * 100, labels[2])
            }
        } else {
            sprintf("%.1f%%", sigmoid(y) * 100)
        }
    } else {
        if (!is.null(labels)) {
            sprintf("%g %s", y, labels[1])
        } else {
            sprintf("%g", y)
        }
    }
}

string_title <- function(title = NULL, y = NULL, logit = FALSE, labels = NULL) {
    if (title == "") {
        NULL
    } else if (!is.null(y) && grepl("%s", title)) {
        sprintf(title, string_prediction(y, logit, labels))
    } else {
        title
    }
}

summary.slise <- print.slise

#' Plot the robust regression or explanation from slise
#'
#' @param x The slise object
#' @param type The type of plot ("2D", "bar", "distribution", "mnist", "prediction", "wordcloud")
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param ... Other parameters to the plotting functions
#' @inheritDotParams plot.slise_2d labels partial size
#' @inheritDotParams plot.slise_bar labels partial size
#' @inheritDotParams plot.slise_distribution labels partial signif
#' @inheritDotParams plot.slise_mnist labels partial width height plots
#' @inheritDotParams plot.slise_prediction labels partial
#' @inheritDotParams plot.slise_wordcloud labels treshold local
#'
#' @return plot or ggplot2 objects
#' @export
#'
#' @examples
#' X <- matrix(rnorm(30), 30, 1)
#' Y <- runif(30, 0, 1)
#' plot(slise.fit(X, Y, epsilon = 0.1))
plot.slise <- function(x,
                       type = NULL,
                       title = NULL,
                       ...) {
    slise <- x
    if (length(type) == 0) {
        if (length(slise$X) == length(slise$Y)) {
            type <- "2d"
        } else if (sparsity(slise$coefficients) > 10) {
            type <- "bar"
        } else {
            type <- "distribution"
        }
    } else {
        type <- tolower(type)
    }
    if (is.null(title)) {
        if (is.null(slise$x)) {
            title <- "SLISE Regression"
        } else if (slise$logit) {
            title <- sprintf("SLISE Explanation (p = %g)", sigmoid(slise$y))
        } else {
            title <- sprintf("SLISE Explanation (y = %g)", slise$y)
        }
    }
    if (type == "plot" || type == "2d") {
        plot.slise_2d(slise, title, ...)
    } else if (type == "distribution" || type == "dist") {
        plot.slise_distribution(slise, title, ...)
    } else if (type == "bar") {
        plot.slise_bar(slise, title, ...)
    } else if (type == "mnist" || type == "emnist" || type == "image" || type == "img") {
        plot.slise_mnist(slise, title, ...)
    } else if (type == "pred" || type == "prediction") {
        plot.slise_prediction(slise, title, ...)
    } else if (type == "wordcloud" || type == "word" || type == "words") {
        plot.slise_wordcloud(slise, title, ...)
    } else {
        stop("[plot.slise] Unknown plot type")
    }
}

#' Plot the robust regression or explanation from slise in 2D
#'
#' @param slise The slise object
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param labels The axis labels (default: c("X", "Y") or c("x", "f(x)"))
#' @param partial Should the raw ggplot2 objects be returned instead of directly plotting (default: FALSE)
#' @param size The size of the plot elements (default: 2)
#' @param ... Ignored parameters
#'
#' @return ggplot object or plot
#'
plot.slise_2d <- function(slise,
                          title = NULL,
                          labels = NULL,
                          partial = FALSE,
                          size = 2,
                          ...) {
    check_package("ggplot2")
    minx <- min(slise$X)
    maxx <- max(slise$X)
    deltax <- maxx - minx
    miny <- min(slise$Y)
    maxy <- max(slise$Y)
    px <- c(
        minx - deltax * 0.1,
        maxx + deltax * 0.1,
        maxx + deltax * 0.1,
        minx - deltax * 0.1
    )
    py <- c(
        sum(c(1, px[1]) * slise$coefficients) + slise$epsilon,
        sum(c(1, px[2]) * slise$coefficients) + slise$epsilon,
        sum(c(1, px[3]) * slise$coefficients) - slise$epsilon,
        sum(c(1, px[4]) * slise$coefficients) - slise$epsilon
    )
    if (is.null(labels)) {
        if (is.null(slise$x)) {
            labels <- c("X", "Y")
        } else {
            labels <- c("x", "f(x)")
        }
    }
    gg <- ggplot2::ggplot() +
        ggplot2::ggtitle(string_title(title, slise$y, slise$logit)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(labels[1]) +
        ggplot2::ylab(labels[2]) +
        ggplot2::coord_cartesian(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
        ggplot2::geom_polygon(ggplot2::aes_string(x = "px", y = "py"), fill = SLISE_WEAKPURPLE) +
        ggplot2::geom_abline(ggplot2::aes(
            intercept = slise$coefficients[1],
            slope = slise$coefficients[2],
            color = "SLISE",
            linetype = "SLISE"
        ), size = size) +
        ggplot2::geom_point(ggplot2::aes(x = slise$X, y = slise$Y), size = size) +
        ggplot2::scale_linetype_manual(
            limits = c("SLISE", "Explained Point"),
            values = c("solid", "blank"),
            name = NULL
        ) +
        ggplot2::scale_color_manual(
            limits = c("SLISE", "Explained Point"),
            values = c(SLISE_PURPLE, SLISE_ORANGE),
            name = NULL
        ) +
        ggplot2::scale_shape_manual(
            limits = c("SLISE", "Explained Point"),
            values = c(NA, 16),
            name = NULL
        )
    if (!is.null(slise$x)) {
        gg <- gg + ggplot2::geom_point(ggplot2::aes(
            x = slise$x,
            y = slise$y,
            color = "Explained Point",
            shape = "Explained Point"
        ), size = size * 2)
    } else {
        gg <- gg + ggplot2::guides(shape = FALSE, color = FALSE, linetype = FALSE)
    }
    if (partial) {
        gg
    } else {
        plot(gg)
    }
}

#' Plot the robust regression or explanation from slise with distributions
#'
#' @param slise The slise object
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param labels The class labels (vector with two strings: c(y_low, y_high), default: c("Low", "High"))
#' @param partial Should the raw ggplot2 objects be returned instead of directly plotting (default: FALSE)
#' @param signif The number of significant digits to display (default: 3)
#' @param ... Ignored parameters
#'
#' @return List of ggplot objects or plot
#'
#' @importFrom stats predict
#'
plot.slise_distribution <- function(slise,
                                    title = NULL,
                                    labels = c("Low", "High"),
                                    partial = FALSE,
                                    signif = 3,
                                    ...) {
    check_package("ggplot2")
    # Distribution plot
    variable_names <- names(slise$coefficients)
    ord <- order_coefficients(slise)
    ord1 <- c(1, ord + 1)
    # Distributions
    label_factors <- c("All", "Subset", "Predicted")
    label_factors <- factor(label_factors, label_factors)
    subset_label <- label_factors[c(rep(1, length(slise$Y)), rep(2, sum(slise$subset)))]
    df <- data.frame(
        x = c(slise$Y, slise$Y[slise$subset], predict(slise, slise$X[slise$subset, ])),
        p = ifelse(is.null(slise$y), NA, slise$y),
        s = label_factors[c(subset_label, rep(3, sum(slise$subset)))],
        l = "Response"
    )
    for (i in ord) {
        df2 <- data.frame(
            x = c(slise$X[, i], slise$X[slise$subset, i]),
            p = ifelse(is.null(slise$x), NA, slise$x[i]),
            s = subset_label,
            l = variable_names[i + slise$intercept]
        )
        df <- rbind(df, df2)
    }
    df$l <- factor(df$l, unique(df$l))
    gg1 <- ggplot2::ggplot(df) +
        ggplot2::facet_wrap(
            "l",
            ncol = 1,
            scales = "free",
            strip.position = "right"
        ) +
        ggplot2::theme_bw() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::geom_density(ggplot2::aes_string("x", "..count..", fill = "s", linetype = "s"), adjust = 0.5) +
        ggplot2::theme(
            legend.position = "bottom",
            strip.text.y = ggplot2::element_text(angle = 90)
        ) +
        ggplot2::ggtitle("Dataset Distribution") +
        ggplot2::scale_fill_manual(
            values = c("white", SLISE_PURPLE, SLISE_WEAKPURPLE),
            limits = c("All", "Subset", "Predicted"),
            name = NULL
        ) +
        ggplot2::scale_linetype_manual(
            values = c(1, 1, 2),
            limits = c("All", "Subset", "Predicted"),
            name = NULL
        )
    if (!is.null(slise$x)) {
        gg1 <- gg1 +
            ggplot2::geom_vline(
                ggplot2::aes_string(xintercept = "p", color = '"Explained"')
            ) +
            ggplot2::scale_color_manual(
                limits = "Explained",
                values = SLISE_ORANGE,
                name = NULL
            )
    }
    # Bars
    if (slise$intercept) {
        mv <- max(abs(slise$coefficients))
        df <- data.frame(
            l = variable_names[ord1],
            x = slise$coefficients[ord1] / mv,
            v = slise$coefficients[ord1],
            r = "Coefficients"
        )
        if (!is.null(slise$normalised)) {
            mv <- max(abs(slise$normalised[ord1]))
            df <- rbind(df, data.frame(
                l = variable_names[ord1],
                x = slise$normalised[ord1] / mv,
                v = slise$normalised[ord1],
                r = "Normalised\nModel"
            ))
        }
    } else {
        mv <- max(abs(slise$coefficients))
        df <- data.frame(
            l = c(" ", variable_names[ord]),
            x = c(0, slise$coefficients[ord] / mv),
            v = c(0, slise$coefficients[ord]),
            r = "Coefficients"
        )
        if (!is.null(slise$normalised)) {
            mv <- max(abs(slise$normalised[ord]))
            df <- rbind(df, data.frame(
                l = c(" ", variable_names[ord]),
                x = c(0, slise$normalised[ord] / mv),
                v = c(0, slise$normalised[ord]),
                r = "Normalised\nModel"
            ))
        }
    }
    if (!is.null(slise$terms)) {
        mv <- max(abs(slise$terms[-1][ord]))
        df <- rbind(df, data.frame(
            l = variable_names[-1][ord],
            x = slise$terms[-1][ord] / mv,
            v = slise$terms[-1][ord],
            r = "Prediction\nTerm"
        ))
        if (!is.null(slise$normalised_terms)) {
            mv <- max(abs(slise$normalised_terms[-1][ord]))
            df <- rbind(df, data.frame(
                l = variable_names[-1][ord],
                x = slise$normalised_terms[-1][ord] / mv,
                v = slise$normalised_terms[-1][ord],
                r = "Normalised\nTerm"
            ))
        }
    }
    df$r <- factor(df$r, c("Normalised\nTerm", "Prediction\nTerm", "Normalised\nModel", "Coefficients"))
    df$l <- factor(df$l, df$l[seq_along(ord1)])
    if (nrow(df) <= length(ord1)) {
        df$f <- labels[(df$x > 0) + 1]
        fill <- ggplot2::scale_fill_manual(
            values = c(SLISE_ORANGE, SLISE_PURPLE),
            name = "Towards: "
        )
    } else {
        df$f <- df$r
        fill <- ggplot2::scale_fill_manual(
            values = c(SLISE_PURPLE, SLISE_DARKPURPLE, SLISE_ORANGE, SLISE_DARKORANGE),
            breaks = rev(levels(df$r)),
            name = NULL
        )
    }
    df$lab <- signif2(df$v, signif)
    gg2 <- ggplot2::ggplot(df) +
        ggplot2::facet_wrap(ggplot2::vars(df$l), ncol = 1, strip.position = "left") +
        ggplot2::theme_bw() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::ggtitle("Linear Model") +
        ggplot2::geom_col(ggplot2::aes_string("r", "x", fill = "f")) +
        ggplot2::coord_flip() +
        fill +
        ggplot2::scale_y_continuous(limits = c(-1, 1)) +
        ggplot2::geom_text(ggplot2::aes_string("r", "x", label = "lab"), hjust = "inward") +
        ggplot2::theme(
            panel.grid.minor.x = ggplot2::element_blank(),
            legend.position = "bottom",
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text.y = ggplot2::element_text(angle = 180)
        )
    if (is.null(slise$normalised) && is.null(slise$x)) {
        gg2 <- gg2 + ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
        )
    }
    # Output
    if (partial) {
        list(gg1, gg2)
    } else {
        check_package("grid")
        check_package("gridExtra")
        title <- string_title(title, slise$y, slise$logit, labels)
        if (is.null(title)) {
            gridExtra::grid.arrange(gg1, gg2, ncol = 2)
        } else {
            gridExtra::grid.arrange(
                gg1,
                gg2,
                ncol = 2,
                top = grid::textGrob(title, gp = grid::gpar(cex = 1.2))
            )
        }
    }
}

#' Plot the robust regression or explanation from slise based on predictions
#'
#' @param slise The slise object
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param labels The axis labels (default: c("Response", "Count"))
#' @param partial Should the raw ggplot2 objects be returned instead of directly plotting (default: FALSE)
#' @param approximation Should the approximation density be added (default: TRUE)
#' @param signif The number of significant digits to display (default: 3)
#' @param ... Ignored parameters
#'
#' @return ggplot object or plot
#'
#' @importFrom stats predict
#'
plot.slise_prediction <- function(slise,
                                  title = NULL,
                                  labels = c("Response", "Count"),
                                  partial = FALSE,
                                  approximation = TRUE,
                                  signif = 3,
                                  ...) {
    check_package("ggplot2")
    # Distributions
    label_factors <- c("Dataset", "Subset", if (approximation) "Approximation" else NULL)
    label_factors <- factor(label_factors, label_factors)
    df <- data.frame(
        x = c(
            slise$Y,
            slise$Y[slise$subset],
            if (approximation) predict(slise, slise$X[slise$subset, ], logit = TRUE) else NULL
        ),
        p = ifelse(is.null(slise$y), NA, slise$y),
        s = label_factors[c(
            rep(1, length(slise$Y)),
            rep(2, sum(slise$subset)),
            if (approximation) rep(3, sum(slise$subset)) else NULL
        )]
    )
    gg1 <- ggplot2::ggplot(df) +
        ggplot2::theme_bw() +
        ggplot2::xlab(labels[1]) +
        ggplot2::ylab(labels[2]) +
        ggplot2::geom_density(ggplot2::aes_string("x", "..count..", fill = "s", linetype = "s")) +
        ggplot2::theme(
            legend.position = "right",
            strip.text.y = ggplot2::element_text(angle = 0)
        ) +
        ggplot2::scale_fill_manual(
            values = c("white", SLISE_PURPLE, if (approximation) SLISE_WEAKPURPLE else NULL),
            limits = c("Dataset", "Subset", if (approximation) "Approximation" else NULL),
            name = NULL
        ) +
        ggplot2::scale_linetype_manual(
            values = c(1, 1, if (approximation) 2 else NULL),
            limits = c("Dataset", "Subset", if (approximation) "Approximation" else NULL),
            name = NULL
        ) +
        ggplot2::ggtitle(string_title(title, slise$y, slise$logit)) +
        ggplot2::scale_x_continuous(labels = function(x) {
            if (slise$logit) base::signif(sigmoid(x), signif) else base::signif(x, signif)
        })
    # Explained Line
    if (!is.null(slise$x)) {
        gg1 <- gg1 +
            ggplot2::geom_vline(
                ggplot2::aes_string(xintercept = "p", color = '"Explained\nPrediction"')
            ) +
            ggplot2::scale_color_manual(
                limits = "Explained\nPrediction",
                values = SLISE_ORANGE,
                name = NULL
            )
    }
    # Output
    if (partial) {
        gg1
    } else {
        plot(gg1)
    }
}

#' Plot the robust regression or explanation from slise as bar plots
#'
#' @param slise The slise object
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param labels The class labels (vector with two strings: c(y_low, y_high), default: c("Low", "High"))
#' @param partial Should the raw ggplot2 objects be returned instead of directly plotting (default: FALSE)
#' @param size The size of the segments (default: 8)
#' @param ... Ignored parameters
#'
#' @return List of ggplot objects or plot
#'
#' @importFrom stats quantile
#'
plot.slise_bar <- function(slise,
                           title = NULL,
                           labels = c("Low", "High"),
                           partial = FALSE,
                           size = 8,
                           ...) {
    check_package("ggplot2")
    ord <- order_coefficients(slise)
    ord1 <- c(1, ord + 1)
    # Dataset
    if (slise$intercept) {
        variable_names <- c("Response", names(slise$coefficients)[ord + 1])
    } else {
        variable_names <- names(slise$coefficients)[ord]
    }
    variable_names <- factor(variable_names, rev(variable_names))
    df <- data.frame(
        low = simplify2array(c(
            quantile(slise$Y, 0.05),
            sapply(ord, function(i) quantile(slise$X[, i], 0.05)),
            quantile(slise$Y, 0.25),
            sapply(ord, function(i) quantile(slise$X[, i], 0.25)),
            quantile(slise$Y[slise$subset], 0.05),
            sapply(ord, function(i) quantile(slise$X[slise$subset, i], 0.05)),
            quantile(slise$Y[slise$subset], 0.25),
            sapply(ord, function(i) quantile(slise$X[slise$subset, i], 0.25))
        )),
        high = simplify2array(c(
            quantile(slise$Y, 0.95),
            sapply(ord, function(i) quantile(slise$X[, i], 0.95)),
            quantile(slise$Y, 0.75),
            sapply(ord, function(i) quantile(slise$X[, i], 0.75)),
            quantile(slise$Y[slise$subset], 0.95),
            sapply(ord, function(i) quantile(slise$X[slise$subset, i], 0.95)),
            quantile(slise$Y[slise$subset], 0.75),
            sapply(ord, function(i) quantile(slise$X[slise$subset, i], 0.75))
        )),
        variable = c(variable_names, variable_names, variable_names, variable_names),
        col = c(
            rep(c("Data   90%", "Data   50%"), each = length(variable_names)),
            rep(c("Subset 90%", "Subset 50%"), each = length(variable_names))
        ),
        height = c(rep(size, length(variable_names) * 2), rep(size * 0.5, length(variable_names) * 2)),
        stringsAsFactors = TRUE
    )
    if (!is.null(slise$x)) {
        df$point <- c(slise$y, slise$x[ord])
        col <- c("black", "grey", SLISE_ORANGE, SLISE_DARKPURPLE, SLISE_PURPLE)
    } else {
        col <- c("black", "grey", SLISE_DARKPURPLE, SLISE_PURPLE)
    }
    gg1 <- ggplot2::ggplot(df) +
        ggplot2::geom_segment(
            ggplot2::aes_string(
                x = "low",
                xend = "high",
                y = "variable",
                yend = "variable",
                col = "col"
            ),
            size = df$height
        ) +
        ggplot2::scale_color_manual(values = col, name = "Distribution") +
        ggplot2::scale_y_discrete(limits = rev(variable_names)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "left") +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::ggtitle("Dataset")
    if (!is.null(slise$x)) {
        gg1 <- gg1 + ggplot2::geom_point(
            ggplot2::aes_string("point", "variable", col = '"Explained"'),
            size = size * 0.5
        )
    }
    # Model
    if (slise$intercept) {
        levels(variable_names)[length(variable_names)] <- "Intercept"
        df <- data.frame(
            alpha = slise$coefficients[ord1],
            names = variable_names
        )
    } else {
        levels(variable_names)[length(variable_names)] <- ""
        df <- data.frame(
            alpha = c(0, slise$coefficients[ord]),
            names = variable_names
        )
    }
    df$labels <- labels[(df$alpha > 0) + 1]
    gg2 <- ggplot2::ggplot(df) +
        ggplot2::geom_col(
            ggplot2::aes_string("names", "alpha", fill = "labels")
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::ggtitle("Linear Model") +
        ggplot2::scale_fill_manual(
            values = c("#e9a3c9", "#a1d76a"),
            limits = labels,
            name = "Towards: "
        )
    # Normalised
    if (!is.null(slise$normalised)) {
        if (slise$intercept) {
            df <- data.frame(
                alpha = slise$normalised[ord1],
                names = variable_names
            )
        } else {
            df <- data.frame(
                alpha = c(0, slise$normalised[ord]),
                names = variable_names
            )
        }
        df$labels <- labels[(df$alpha > 0) + 1]
        gg3 <- ggplot2::ggplot(df) +
            ggplot2::geom_col(
                ggplot2::aes_string("names", "alpha", fill = "labels")
            ) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw() +
            ggplot2::theme() +
            ggplot2::xlab(NULL) +
            ggplot2::ylab(NULL) +
            ggplot2::ggtitle("Normalised Model") +
            ggplot2::scale_fill_manual(
                values = c("#e9a3c9", "#a1d76a"),
                limits = labels,
                name = "Towards: "
            ) +
            ggplot2::theme(
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            )
        gg2 <- gg2 + ggplot2::theme(legend.position = "none")
        # Output
        out <- list(gg1, gg2, gg3)
    } else {
        out <- list(gg1, gg2)
    }
    if (partial) {
        out
    } else {
        check_package("grid")
        check_package("gridExtra")
        out$ncol <- length(out)
        title <- string_title(title, slise$y, slise$logit, labels)
        if (!is.null(title)) out$top <- grid::textGrob(title, gp = grid::gpar(cex = 1.2))
        do.call(gridExtra::grid.arrange, out)
    }
}

#' Plot the robust regression or explanation from slise as an image
#'
#' @param slise The slise object
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param labels The class labels (vector with two strings: c(y_low, y_high), default: c("Low", "High"))
#' @param partial Should the raw ggplot2 objects be returned instead of directly plotting (default: FALSE)
#' @param width The width of the image (width * height == ncol(X))
#' @param height The height of the image (width * height == ncol(X))
#' @param plots The number of plots to split the explanation into (default: 1)
#' @param enhance_colours Increse the saturation of the explanation (default: TRUE)
#' @param ... Ignored parameters
#' @param breaks Breaks for the countours, see `ggplot2::stat_contour` (default: NULL)
#'
#' @return ggplot object(s) or plot
#'
plot.slise_mnist <- function(slise,
                             title = NULL,
                             labels = c("Low", "High"),
                             partial = FALSE,
                             width = floor(sqrt(ncol(slise$X))),
                             height = width,
                             plots = 1,
                             enhance_colours = TRUE,
                             ...,
                             breaks = NULL) {
    check_package("ggplot2")
    if (is.null(slise$x)) {
        plots <- 1
    }
    title <- string_title(title, slise$y, slise$logit, labels)
    if (plots == 1) {
        gg <- plot_mnist(
            matrix(slise$coefficients[-1], height, width),
            if (is.null(slise$x)) NULL else matrix(slise$x, height, width),
            labels,
            enhance_colours = enhance_colours,
            breaks = breaks
        ) +
            ggplot2::ggtitle(title)
        if (partial) {
            gg
        } else {
            plot(gg)
        }
    } else if (plots == 2) {
        gg1 <- plot_mnist(
            matrix(slise$x, height, width),
            colours = c("white", "black"),
            enhance_colours = FALSE
        ) +
            ggplot2::ggtitle("Explained Image") +
            ggplot2::theme(legend.position = "left")
        gg2 <- plot_mnist(
            matrix(slise$coefficients[-1], height, width),
            matrix(slise$x, height, width),
            labels,
            enhance_colours = enhance_colours,
            breaks = breaks
        ) +
            ggplot2::ggtitle("Explanation") +
            ggplot2::theme(legend.position = "right")
        if (partial) {
            list(gg1, gg2)
        } else {
            check_package("grid")
            check_package("gridExtra")
            if (is.null(title)) {
                gridExtra::grid.arrange(gg1, gg2, ncol = 2)
            } else {
                gridExtra::grid.arrange(
                    gg1,
                    gg2,
                    ncol = 2,
                    top = grid::textGrob(title, gp = grid::gpar(cex = 1.2))
                )
            }
        }
    } else if (plots == 3) {
        gg1 <- plot_mnist(
            matrix(slise$x, height, width),
            colours = c("white", "black"),
            enhance_colours = FALSE
        ) +
            ggplot2::ggtitle("Explained Image") +
            ggplot2::theme(
                legend.position = "none",
                plot.title = ggplot2::element_text(hjust = 0.5)
            )
        gg2 <- plot_mnist(
            matrix(pmin(slise$coefficients[-1], 0), height, width),
            matrix(slise$x, height, width),
            labels,
            enhance_colours = enhance_colours,
            breaks = breaks
        ) +
            ggplot2::ggtitle(paste("Towards", labels[1])) +
            ggplot2::theme(
                legend.position = "none",
                plot.title = ggplot2::element_text(hjust = 0.5)
            )
        gg3 <- plot_mnist(
            matrix(pmax(slise$coefficients[-1], 0), height, width),
            matrix(slise$x, height, width),
            labels,
            enhance_colours = enhance_colours,
            breaks = breaks
        ) +
            ggplot2::ggtitle(paste("Towards", labels[2])) +
            ggplot2::theme(
                legend.position = "none",
                plot.title = ggplot2::element_text(hjust = 0.5)
            )
        if (partial) {
            list(gg1, gg2, gg3)
        } else {
            check_package("grid")
            check_package("gridExtra")
            if (is.null(title)) {
                gridExtra::grid.arrange(gg1, gg3, gg2, ncol = 3)
            } else {
                gridExtra::grid.arrange(
                    gg1,
                    gg3,
                    gg2,
                    ncol = 3,
                    top = grid::textGrob(title, gp = grid::gpar(cex = 1.2))
                )
            }
        }
    } else {
        stop("Unimplemented number of plots")
    }
}

#' @importFrom stats median
plot_mnist <- function(image,
                       contour = NULL,
                       labels = NULL,
                       colours = c(SLISE_DARKORANGE, "white", SLISE_DARKPURPLE),
                       enhance_colours = TRUE,
                       breaks = NULL) {
    check_package("reshape2")
    check_package("ggplot2")
    shape <- dim(image)
    if (enhance_colours) {
        image <- sigmoid(3 * image / max(abs(image))) * 2 - 1
        dim(image) <- shape
    }
    if (length(colours) == 3) {
        limits <- c(-1, 1) * max(abs(image))
    } else {
        limits <- c(min(image), max(image))
    }
    gg <- ggplot2::ggplot(reshape2::melt(image)) +
        ggplot2::geom_raster(
            if (length(shape) == 3) {
                ggplot2::aes_string("Var3", "Var2", fill = "value")
            } else {
                ggplot2::aes_string("Var2", "Var1", fill = "value")
            },
            interpolate = FALSE
        ) +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            aspect.ratio = shape[length(shape) - 1] / shape[length(shape)]
        ) +
        ggplot2::scale_y_reverse(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(expand = c(0, 0))
    if (is.null(labels)) {
        gg <- gg + ggplot2::scale_fill_gradientn(
            colours = colours,
            name = NULL,
            limits = limits,
            breaks = if (enhance_colours) NULL else ggplot2::waiver()
        )
    } else {
        gg <- gg + ggplot2::scale_fill_gradientn(
            colours = colours,
            name = NULL,
            limits = limits,
            labels = labels,
            breaks = limits * 0.8,
            guide = ggplot2::guide_legend(title = NULL, reverse = TRUE)
        )
    }
    if (!is.null(contour)) {
        if (length(dim(image)) == 3 && length(contour) == dim(image)[2] * dim(image)[3]) {
            contour <- rep(contour, dim(image)[1])
            dim(contour) <- dim(image)[c(2, 3, 1)]
            contour <- aperm(contour, c(3, 1, 2))
        }
        stopifnot(dim(contour) == dim(image))
        gg <- gg + ggplot2::stat_contour(
            if (length(shape) == 3) {
                ggplot2::aes_string("Var3", "Var2", z = "value")
            } else {
                ggplot2::aes_string("Var2", "Var1", z = "value")
            },
            data = reshape2::melt(contour),
            col = "black",
            bins = 2,
            breaks = breaks
        )
    }
    gg
}

#' Plot the robust regression or explanation from slise as a wordcloud
#'
#' @param slise The slise object
#' @param title The title of the plot (may include a `\%s`, which will be replaced by the prediction)
#' @param labels The class labels (vector with two strings: c(y_low, y_high), default: c("Low", "High"))
#' @param treshold Treshold for ignored value (default: 1e-8)
#' @param local Only display the words relevant for the explained item (default: TRUE)
#' @param ... Ignored parameters
#'
#' @return plot
#'
#' @importFrom graphics legend
#'
plot.slise_wordcloud <- function(slise,
                                 title = NULL,
                                 labels = c("Low", "High"),
                                 treshold = 1e-8,
                                 local = TRUE,
                                 ...) {
    check_package("wordcloud")
    mask <- abs(slise$alpha) > treshold
    if (local && !is.null(slise$x)) {
        if (length(mask) > length(slise$x)) {
            mask[1] <- FALSE
            mask[-1] <- mask[-1] & (abs(slise$x) > treshold)
        } else {
            mask <- mask & (abs(slise$x) > treshold)
        }
    } else if (slise$intercept) {
        mask[1] <- FALSE
    }
    wordcloud::wordcloud(
        names(slise$coefficients)[mask],
        abs(slise$alpha[mask]),
        colors = ifelse(slise$alpha[mask] > 0, SLISE_PURPLE, SLISE_ORANGE),
        ordered.colors = TRUE
    )
    legend(
        "bottom",
        legend = labels,
        text.col = c(SLISE_ORANGE, SLISE_PURPLE),
        horiz = TRUE,
        title = "Towards:",
        title.col = "black",
        bty = "n",
        adj = 0.5,
        x.intersp = 2
    )
    title(string_title(title, slise$y, slise$logit, labels))
}