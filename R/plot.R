# This script contains functions for plotting, printing and showing SLISE regressions and explanations

#' Plot the robust regression or explanation from slise
#'
#' @param slise The slise object
#' @param cols The columns in the data to plot
#' @param title (Optional) The title of the plot (and result names when using other)
#' @param labels (Optional) The labels for the x, y, and legend (in that order, can be partial)
#' @param other list of other slise objects to include in the plot
#' @param threed plot in 3D with two columns
#' @param ... not used
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(200), 100, 2)
#' response <- rnorm(100)
#' slise <- slise.fit(data, response, epsilon=0.1)
#' plot(slise, 1:2, threed = TRUE)
plot.slise <- function(slise, cols = 1, title = "SLISE", labels = NULL, other = NULL, threed = FALSE, ...) {
    if (length(cols) == 1) {
        x <- c(min(slise$X[, cols]), max(slise$X[, cols]))
        if (length(labels) == 1)
            labels <- c(colnames(slise$data)[[cols]])
        if (is.null(other)) {
            y <- slise$coefficients[[1]] + slise$coefficients[[cols + 1]] * x
            plt <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(slise$X[, cols], slise$Y)) +
                ggplot2::geom_line(ggplot2::aes(x, y), color = "#e66101", size = 1) +
                ggplot2::geom_point(ggplot2::aes(slise$X[slise$subset, cols], slise$Y[slise$subset]), color = "#5e3c99") +
                ggplot2::ggtitle(title) + ggplot2::theme_light()
        } else {
            other <- append(list(slise), other)
            if (is.null(title)) title <- ""
            if (length(title) < length(other) + 1)
                title <- c(title, paste(length(title):length(other)))
            explanations <- data.frame(
                y = c(sapply(other, function(e) e$coefficients[[1]] + e$coefficients[[cols + 1]] * x)),
                x = c(rep(x, length(other))),
                title = title[-1])
            plt <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(slise$X[, cols], slise$Y)) +
                ggplot2::geom_point(ggplot2::aes(slise$X[slise$subset, cols], slise$Y[slise$subset]), color = "gray42") +
                ggplot2::geom_line(ggplot2::aes(explanations$x, explanations$y, col = explanations$title)) +
                ggplot2::ggtitle(title[[1]])
        }
        if (length(labels) == 1)
            plt <- plt + ggplot2::labs(x = labels[[1]])
        else if (length(labels) == 2)
            plt <- plt + ggplot2::labs(x = labels[[1]], y = labels[[2]])
        else if (length(labels) == 3)
            plt <- plt + ggplot2::labs(x = labels[[1]], y = labels[[2]], col = labels[[3]])
        if (!is.null(slise$x) && !is.null(slise$y)) {
            plt <- plt + ggplot2::geom_point(ggplot2::aes(slise$x[[cols]], slise$y), col = "red")
        }
        graphics::plot(plt)
    } else if (length(cols) == 2) {
        if (!is.null(other))
            warning("Multiple slise objects only allowed when plotting one column")
        x_var <- cols[[1]]
        y_var <- cols[[2]]
        zlab <- labels[[1]]
        if (length(labels) > 1) xlab <- labels[[2]]
        else xlab <- colnames(slise$data)[[x_var]]
        if (length(labels) > 2) ylab <- labels[[3]]
        else ylab <- colnames(slise$data)[[y_var]]
        if (!threed) {
            plt <- ggplot2::ggplot() +
                ggplot2::geom_point(ggplot2::aes(slise$X[, x_var], slise$X[, y_var], col = slise$Y)) +
                ggplot2::geom_point(ggplot2::aes(slise$X[slise$subset, x_var], slise$X[slise$subset, y_var]), col = "green") +
                ggplot2::ggtitle(title) + ggplot2::labs(x = xlab, y = ylab, col = zlab)
            if (!is.null(slise$x)) {
                plt <- plt + ggplot2::geom_point(ggplot2::aes(slise$x[[x_var]], slise$x[[y_var]]), col = "red")
            }
            graphics::plot(plt)
        } else {
            if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
                stop("Package \"scatterplot3d\" needed for 3D plots. Please install it.",
                call. = FALSE)
            }
            plt <- scatterplot3d::scatterplot3d(slise$X[, x_var], slise$X[, y_var], slise$Y, xlab = xlab, ylab = ylab,
                zlab = zlab, pch = 16, color = grDevices::rgb(0.2, 0.2, 0.8, 0.3), main = title)
            plt$points3d(slise$X[slise$subset, x_var], slise$X[slise$subset, y_var],
                slise$Y[slise$subset], pch = 16, col = "green")
            if (!is.null(slise$x) && !is.null(slise$y)) {
                plt$points3d(slise$x[[x_var]], slise$x[[y_var]], slise$y, pch = 16, col = "red")
            }
            plt$plane3d(slise$coefficients[c(1, x_var + 1, y_var + 1)])
        }
    } else {
        stop("plot.slise not defined for more than two variables")
    }
}

#' Print the robust regression or explanation from slise
#'
#' @param slise The slise object
#' @param title (Optional) The title of the result
#' @param ... not used
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(200), 100, 2)
#' response <- rnorm(100)
#' slise <- slise.fit(data, response, epsilon=0.1)
#' print(slise)
print.slise <- function(slise, ..., title = "SLISE") {
    Coefficients <- slise$coefficients
    Alpha <- slise$scaled$expand_alpha(slise$alpha)
    if (length(Alpha) < length(Coefficients))
        Alpha <- c(NA, Alpha)
    if (!is.null(slise$x)) {
        Explained <- c(NA, slise$x)
        xs <- slise$scaled$scale_x(slise$x)
        Contribution <- c(slise$alpha[[1]], slise$scaled$expand_alpha(slise$alpha[-1] * xs))
        data <- rbind(Coefficients, Alpha, Explained, Contribution)
    }
    else {
        data <- rbind(Coefficients, Alpha)
    }
    colnames(data) <- names(slise$coefficients)
    if (ncol(data) > 20) {
        data <- data[, c(TRUE, slise$x != 0)]
        if (ncol(data) > 20) data <- data[, 1:20]
    }
    if (!is.null(title)) {
        cat(title, "\n", sep = "")
    }
    print(data)
    cat(sprintf("Subset size: %6.2f\n", mean(slise$subset)))
    cat(sprintf("Loss:        %6.2f\n", slise$value))
    cat(sprintf("Epsilon:     %6.2f\n", slise$epsilon))
    cat(sprintf("Lambda:      %6.2f\n", slise$lambda))
    cat(sprintf("Non-Zero:    %3d\n", sparsity(slise$alpha)))
    if (length(slise$logit) == 1 && slise$logit)
        cat(sprintf("Class Balance: %.1f%% / %.1f%%\n", mean(slise$scaled$Y[slise$subset] > 0)*100, mean(slise$scaled$Y[slise$subset] < 0)*100))
    invisible(slise)
}

summary.slise <- print.slise

#' Explain an Object
#'
#' @param x the object to explain
#' @param ... additional parameters
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(200), 100, 2)
#' response <- rnorm(100)
#' slise <- slise.explain(data, response, 10, epsilon=0.1)
#' explain(slise)
explain <- function(x, ...) {
    UseMethod("explain", x)
}

#' Show a SLISE explanation
#'
#' @param slise the slise object to show
#' @param type Type of explanation (bar, distribution, image, image2, text, image_scatter)
#' @param class_labels (Optional) names of the two classes
#' @param real_value (Optional) the real response for the explained instance
#' @param title (Optional) A title to add to the explanation
#' @param probability (Optional) is The prediction a probability (TRUE)
#' @param ... Additional parameters to the visualisation
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(200), 100, 2)
#' response <- rnorm(100)
#' slise <- slise.explain(data, response, 10, epsilon=0.1)
#' explain(slise)
explain.slise <- function(slise, type = "bar", class_labels = c("Class 0", "Class 1"),
        title = "SLISE", real_value = NULL, probability = TRUE, ...) {
    if (is.null(type)) type <- ifelse(ncol(slise$X) > 20, "image", "bar")
    if (is.null(slise$x) || is.null(slise$y)) {
        stop("Can only show explanations for actual explanations (use slise.explain)")
    }
    title <- explain_slise_title(slise, main_title = title, class_labels = class_labels, real_value = real_value, probability = probability, ...)
    if (identical(type, "bar")) {
        explain_slise_bar(slise, title = title, class_labels = class_labels)
    } else if (identical(type, "distribution") || identical(type, "dist")) {
        explain_slise_distribution(slise, title = title, class_labels = class_labels)
    } else if (identical(type, "image")) {
        explain_slise_image(slise, title = title, class_labels = class_labels, ...)
    } else if (identical(type, "image2")) {
        explain_slise_image2(slise, title = title, class_labels = class_labels, ...)
    } else if (identical(type, "image_scatter")) {
        explain_slise_image_scatter(slise, title = title, class_labels = class_labels, ...)
    } else if (identical(type, "text")) {
        explain_slise_text(slise, title = title, class_labels = class_labels, ...)
    } else {
        stop("Unknown explanation visualisation type")
    }
}

# Plot a tabular explanation with bar graphs
explain_slise_bar <- function(slise, class_labels = c("", ""), title = "") {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("Package \"gridExtra\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (!requireNamespace("grid", quietly = TRUE)) {
        stop("Package \"grid\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    X <- slise$scaled$X
    Y <- slise$scaled$Y
    x <- slise$scaled$scale_x(slise$x)
    y <- slise$scaled$scale_y(slise$y)
    alpha <- slise$alpha
    # Setup
    mask <- abs(alpha[-1]) > 1e-6
    X <- X[, mask]
    x <- x[mask]
    inter <- alpha[[1]]
    alpha <- alpha[-1][mask]
    var_names <- names(alpha)
    if (is.null(var_names)) var_names <- colnames(X)
    if (is.null(var_names)) var_names <- seq_along(alpha)
    ord <- rev(order(abs(alpha)))
    iord <- rev(c(1, ord + 1))
    # Subset
    dataset_low <- c(stats::quantile(Y, 0.05) * 3, sapply(1:ncol(X), function(i) stats::quantile(X[, i], 0.05)))
    dataset_high <- c(stats::quantile(Y, 0.95) * 3, sapply(1:ncol(X), function(i) stats::quantile(X[, i], 0.95)))
    subset_low <- c(stats::quantile(Y[slise$subset], 0.05) * 3, sapply(1:ncol(X), function(i) stats::quantile(X[slise$subset, i], 0.05)))
    subset_high <- c(stats::quantile(Y[slise$subset], 0.95) * 3, sapply(1:ncol(X), function(i) stats::quantile(X[slise$subset, i], 0.95)))
    subset_point <- c(y * 3, x)
    subset_names <- factor(c(sprintf("Predicted (%.3f)", slise$y), mapply(function(n, v) sprintf("%s (%.3f)", n, v), var_names, slise$x[mask])))
    plt_subset <- ggplot2::ggplot() +
        ggplot2::geom_hline(yintercept = 0, color = "gray32") +
        ggplot2::geom_segment(ggplot2::aes(x = subset_names, xend = subset_names, y = dataset_low, yend = dataset_high, col = "gray"), size = 12) +
        ggplot2::geom_segment(ggplot2::aes(x = subset_names, xend = subset_names, y = subset_low, yend = subset_high, col = "orange"), size = 8) +
        ggplot2::geom_point(ggplot2::aes(subset_names, subset_point, col = "black"), size = 6) + ggplot2::scale_x_discrete(limits = subset_names[iord]) +
        ggplot2::scale_color_identity(name = "", guide = "legend", labels = c("Explained Sample", "Dataset (95%)", "Subset (95%)")) +
        ggplot2::ggtitle(sprintf("Subset Size (%.1f%%)", 100 * mean(slise$subset))) + ggplot2::xlab("") + ggplot2::ylab("") +
        ggplot2::coord_flip() + ggplot2::theme_minimal(14) +
        ggplot2::theme(legend.position = "bottom", axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), legend.box.margin = ggplot2::margin(-40, 80, -10, -40, "pt"))
    # Model
    model <- c(inter, alpha)
    model_sign <- ifelse(model > 0, "+", "-")
    model_names <- factor(mapply(function(l, v) sprintf("%s (%.3f)", l, v), c("Intercept", var_names), model))
    plt_model <- ggplot2::ggplot(mapping = ggplot2::aes(model_names, model, fill = model_sign)) + ggplot2::scale_x_discrete(limits = model_names[iord]) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::scale_fill_manual(values = c("-" = "red2", "+" = "green3"), name = "", labels = c("+" = class_labels[[2]], "-" = class_labels[[1]])) +
        ggplot2::ggtitle("Local Linear Model") + ggplot2::ylab("") + ggplot2::xlab("") + ggplot2::ylim(-max(abs(model)), max(abs(model))) +
        ggplot2::coord_flip() + ggplot2::theme_minimal(14) + ggplot2::theme(legend.position = "bottom", legend.box.margin = ggplot2::margin(-40, 0, -10, 0, "pt"))
    # Impact
    impact <- c(inter, alpha * x)
    impact_sign <- ifelse(impact > 0, "+", "-")
    plt_impact <- ggplot2::ggplot(mapping = ggplot2::aes(model_names, impact, fill = impact_sign)) + ggplot2::scale_x_discrete(limits = model_names[iord]) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::scale_fill_manual(values = c("+" = "green3", "0" = "white", "-" = "red2"), name = "", labels = c("+" = class_labels[[2]], "0" = "", "-" = class_labels[[1]])) +
        ggplot2::ggtitle("Actual Impact") + ggplot2::ylab("") + ggplot2::xlab("") + ggplot2::ylim(-max(abs(impact)), max(abs(impact))) +
        ggplot2::coord_flip() + ggplot2::theme_minimal(14) + ggplot2::theme(legend.position = "bottom", legend.box.margin = ggplot2::margin(-40, 0, -10, 0, "pt"),
            axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5))
    gridExtra::grid.arrange(plt_model, plt_impact, plt_subset, ncol = 3, widths = c(3, 2, 3), top = grid::textGrob(title, gp = grid::gpar(cex = 1.2)))
}

# Plot an EMNIST explanation with a saliency map and outline of the digit
explain_slise_image <- function(slise, title = "", class_labels = c("", ""), width = NULL, height = NULL, ...) {
    if (is.null(width) && is.null(height))
        width <- height <- floor(sqrt(length(slise$x)))
    stopifnot(width * height == length(slise$x))
    alpha <- slise$scaled$expand_alpha(slise$alpha)[-1]
    plt <- explain_img_slise_image(alpha, slise$x, width, height, ..., class_labels = class_labels, legend = "bottom")
    graphics::plot(plt + ggplot2::ggtitle(title))
}

# Plot an EMNIST explanation with a saliency map and the image
explain_slise_image2 <- function(slise, title = "", class_labels = c("", ""), width = NULL, height = NULL, ...) {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("Package \"gridExtra\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (!requireNamespace("grid", quietly = TRUE)) {
        stop("Package \"grid\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (is.null(width) && is.null(height))
        width <- height <- floor(sqrt(length(slise$x)))
    stopifnot(width * height == length(slise$x))
    alpha <- slise$scaled$expand_alpha(slise$alpha)[-1]
    plt <- explain_img_slise_image(alpha, NULL, width, height, ..., class_labels = class_labels, legend = "left")
    plt2 <- explain_img_slise_image(slise$x, NULL, width, height, ..., legend = "right", colors = explain_slise_color_bw(), scale_colors = FALSE)
    gridExtra::grid.arrange(plt + ggplot2::ggtitle("Saliency Map"), plt2 + ggplot2::ggtitle("Image"), ncol=2, top = grid::textGrob(title, gp = grid::gpar(cex = 1.2)))
}

# Plot an EMNIST explanation with a scatterplot and a lineup
explain_slise_image_scatter <- function(slise, title = "", class_labels = c("", ""), width = NULL, height = NULL, num_examples = 5, ...) {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("Package \"gridExtra\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (!requireNamespace("grid", quietly = TRUE)) {
        stop("Package \"grid\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (is.null(width) && is.null(height))
        width <- height <- floor(sqrt(length(slise$x)))
    stopifnot(width * height == length(slise$x))
    alpha <- slise$scaled$expand_alpha(slise$alpha)[-1]
    lineup <- explain_slise_get_lineup(slise, num_examples, TRUE)
    images <- do.call(rbind, lapply(lineup$probabilities, function(p) alpha))
    plt_cmp <- explain_img_slise_lineup(images, paste("p ==", lineup$probabilities), lineup$images, width, height, ..., class_labels = class_labels, legend = "bottom", nrow = 1) +
        ggplot2::ggtitle(paste0("Numbers from the subset with different probabilities ('", class_labels[[1]], "' to '", class_labels[[2]], "')"))
    plt_scatter <- explain_img_slise_scatter(slise, width, height, lineup, ...)
    plt_scatter <- plt_scatter + ggplot2::ggtitle(title)
    sub_bal <- mean(slise$scaled$Y[slise$subset] > 0)
    layout <- matrix(c(1, 1, 2, 1, 1, 2), 3, 2)
    if (sub_bal <= 0.1 || sub_bal >= 0.9) {
        warning <- grid::textGrob("WARNING: Subset is very unbalanced", gp = grid::gpar(col = "dark red"))
        gridExtra::grid.arrange(plt_scatter, plt_cmp, layout_matrix = layout, bottom = warning)
    } else {
        gridExtra::grid.arrange(plt_scatter, plt_cmp, layout_matrix = layout)
    }
}

# Print a text explanation, with optional weights and plotted wordcloud
explain_slise_text <- function(slise, title, class_labels, text = NULL, tokens = NULL, treshold = 1e-2, print_weights = TRUE, print_weights_all = FALSE, wordcloud = TRUE, ...) {
    if (!requireNamespace("crayon", quietly = TRUE)) {
        stop("Package \"crayon\" needed for coloring words. Please install it.",
        call. = FALSE)
    }
    if (is.null(text)) {
        text <- names(slise$x)[slise$x > 0]
    } else if (length(text) == 1) {
        text <- stringr::str_split(text, " ")[[1]]
    }
    if (is.null(tokens))
        tokens <- text
    cat(stringr::str_replace_all(title, ", ", "  \n"), "  \n")
    ns <- mean(slise$Y[slise$subset] < 0.5)
    ps <- mean(slise$Y[slise$subset] > 0.5)
    cat("  Subset Size: ", sprintf("%.2f%% (", mean(slise$subset) * 100),
        crayon::make_style(grDevices::rgb(0.8, 0, 0))(sprintf("%.1f%%", ns * 100)), " + ",
        crayon::make_style(grDevices::rgb(0, 0.7, 0))(sprintf("%.1f%%", ps * 100)), ")",
        ifelse(ns < 0.1 || ps < 0.1, crayon::make_style(grDevices::rgb(1, 0.5, 0))(" UNBALANCED SUBSET!"), ""),
        "  \n", sep = "")
    cat("  Legend:", crayon::make_style(grDevices::rgb(0.8, 0, 0))(class_labels[1]), "Neutral",
        crayon::make_style(grDevices::rgb(0, 0.7, 0))(class_labels[2]), crayon::silver("Unknown"), "  \n")
    vmax <- max(abs(slise$alpha[-1]))
    vmed <- stats::median(abs(slise$alpha[-1]))
    th <- vmax * 0.5 + vmed * 0.5
    treshold <- th * treshold
    color <- function(v, t) {
        if (is.na(v)) crayon::silver(t)
        else if (abs(v) < treshold) t
        else if (v > 0) {
            if (v > th) {
                crayon::make_style(grDevices::rgb(0, 0.7, 0))(t)
            } else {
                v <- (1 - v / th) ^ 2
                crayon::make_style(grDevices::rgb(v, 1, v))(t)
            }
        } else {
            if (v < -th) {
                crayon::make_style(grDevices::rgb(0.8, 0, 0))(t)
            } else {
                v <- (1 + v / th) ^ 2
                crayon::make_style(grDevices::rgb(1, v, v))(t)
            }
        }
    }
    do.call(cat, lapply(seq_along(text), function(i) {
        t <- text[[i]]
        j <- which(tokens[[i]] == names(slise$coefficients))
        v <- if (length(j) == 1) { slise$alpha[j] } else { NA }
        color(v, t)
    }))
    cat("  \n")
    if (print_weights || print_weights_all) {
        ord <- c(1, rev(order(abs(slise$alpha[-1]))) + 1)
        if (!print_weights_all) {
            ord <- ord[c(1, slise$x)[ord] != 0]
        }
        mapply(function(t, v) {
            if (abs(v) < treshold) return();
            t <- sprintf("%10s", t)
            t <- color(v, t)
            cat(sprintf("%s: %7.4f", t, v), "  \n")
        }, names(slise$coefficients)[ord], slise$alpha[ord])
    }
    if (wordcloud) {
        if (!requireNamespace("wordcloud", quietly = TRUE)) {
            stop("Package \"wordcloud\" needed for plotting wordclouds. Please install it.",
            call. = FALSE)
        }
        mask <- c(FALSE, abs(slise$alpha[-1]) > treshold & slise$x != 0)
        wordcloud::wordcloud(names(slise$coefficients)[mask], abs(slise$alpha[mask]),
            colors = ifelse(slise$alpha[mask] > 0, "#4dac26", "#d01c8b"),
            ordered.colors = TRUE, use.r.layout = TRUE)
        graphics::par(mar = rep(0, 4))
    }
    invisible(slise)
}

# Plot a tabular explanation with density plots
explain_slise_distribution <- function(slise, title, class_labels) {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("Package \"gridExtra\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (!requireNamespace("grid", quietly = TRUE)) {
        stop("Package \"grid\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    num_variables <- sparsity(slise$alpha[-1])
    variable_names <- names(slise$coefficients)
    ord <- order(abs(slise$alpha[-1]), decreasing = TRUE)[1:num_variables]
    #Distributions
    plts <- lapply(ord, function(i) {
        ggplot2::ggplot() + ggplot2::xlab(variable_names[[i + 1]]) +
            ggplot2::geom_density(ggplot2::aes(slise$X[, i], ggplot2::stat("count")), color = "red2") +
            ggplot2::geom_density(ggplot2::aes(slise$X[slise$subset, i], ggplot2::stat("count")), color = "cyan3") +
            ggplot2::geom_vline(xintercept = slise$x[[i]], color = "black") +
            ggplot2::geom_point(ggplot2::aes(slise$x[[i]], 0), color = "black", size = 2.5) + ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                axis.title.y = ggplot2::element_blank(), legend.position = "none",
                panel.grid.minor.y = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
    })
    plts <- append(list(
        ggplot2::ggplot() + ggplot2::xlab("response") +
            ggplot2::geom_density(ggplot2::aes(slise$Y, ggplot2::stat("count"), col = "c1")) +
            ggplot2::geom_density(ggplot2::aes(slise$Y[slise$subset], ggplot2::stat("count"), col = "c2")) +
            ggplot2::geom_vline(ggplot2::aes(xintercept = slise$y, col = "c3"), show.legend = TRUE) +
            ggplot2::geom_point(ggplot2::aes(slise$y, 0, col = "c3"), size = 2.5) +
            ggplot2::theme_minimal() + ggplot2::scale_color_manual(name = "Legend: ",
                values = c("red2", "cyan3", "black"),
                labels = c("Dataset", "Subset", "Explained Instance")) +
            ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                axis.title.y = ggplot2::element_blank(), legend.position = "bottom",
                panel.grid.minor.y = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
    ), plts)
    #Meta
    impact <- slise$alpha[-1] * slise$scaled$scale_x(slise$x)
    ml <- max(c(max(abs(impact)), max(abs(slise$alpha))))
    labs <- rev(c("Subset   ", "Size     ", "", "Intercept"))
    labs <- factor(labs, levels = labs)
    mp <- mean(slise$subset) * 2 * ml - ml
    plts2 <- list(
        ggplot2::ggplot(mapping = ggplot2::aes(labs, c(0, 0, 0, 0))) + ggplot2::geom_col() +
        ggplot2::theme_minimal() + ggplot2::scale_y_continuous(limits = c(-ml, ml), breaks = c(-ml, -0.5 * ml, 0, 0.5 * ml, ml) * 0.8,) +
        ggplot2::geom_col(ggplot2::aes(labs[[1]], slise$alpha[[1]], fill = ifelse(slise$alpha[[1]] > 0, "green3", "red2"))) +
        ggplot2::geom_rect(ggplot2::aes(xmin = 1.7, xmax = 2.8, ymin = -ml, ymax = ml), size = 10, fill = "white") +
        ggplot2::geom_vline(xintercept = 2, color = "white", size = 10) +
        ggplot2::geom_text(ggplot2::aes(x = 2.5, y = c(-ml * 0.975, 0, ml * 0.95), label = paste(c(0, 50, 100), "%")), size = 3) +
        ggplot2::geom_rect(ggplot2::aes(xmin = labs[[4]], xmax = labs[[3]], ymin = -ml, ymax = mp), fill = "orange") +
        ggplot2::geom_text(ggplot2::aes(3.5, mp, label = sprintf(" %.1f %%", mean(slise$subset) * 100), hjust = 0)) +
        ggplot2::scale_fill_manual(labels = class_labels, values = c("red2", "green3"), limits = c("red2", "green3"), drop = FALSE, guide = ggplot2::guide_legend(title = NULL)) +
        ggplot2::coord_flip() + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(), rect = ggplot2::element_blank(), legend.position = "bottom", axis.text.x = ggplot2::element_blank())

    )
    # Model
    plts2 <- append(plts2, lapply(ord, function(i) {
        val <- c(slise$alpha[[i + 1]], impact[[i]])
        col <- ifelse(val > 0, "green3", "red2")
        ggplot2::ggplot(mapping = ggplot2::aes(c("Weight   ", "Impact   "), val)) +
        ggplot2::theme_minimal() + ggplot2::scale_y_continuous(limits = c(-ml, ml), breaks = c(-ml, -0.5 * ml, 0, 0.5 * ml, ml) * 0.8) +
        ggplot2::geom_col(ggplot2::aes("Weight   ", val[[1]]), fill = col[[1]]) +
        ggplot2::geom_col(ggplot2::aes("Impact   ", val[[2]]), fill = col[[2]]) +
        ggplot2::coord_flip() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank())
    }))
    plts <- append(plts, plts2)[order(c(seq_along(plts) * 2 - 1, seq_along(plts) * 2))]
    do.call(gridExtra::grid.arrange, append(plts, list(ncol = 2, nrow = length(plts) / 2, heights = c(2, rep(1, length(plts) / 2 - 1)),
        top = grid::textGrob(title, gp = grid::gpar(cex = 1.2)))))
}

# Generate title with additional information
explain_slise_title <- function(slise, real_value = NULL, class_labels = NULL, main_title = NULL, probability = TRUE, ...) {
    title <- list(sep = ",   ")
    if (!is.null(main_title)) {
        title <- c(title, main_title)
    }
    if (!is.null(slise$y)) {
        pred <- slise$y
        if (probability) {
            if (is.null(class_labels))
                pred <- sprintf("%.1f %%", pred * 100)
            else if (pred >= 0.5)
                pred <- sprintf("%.1f%% %s", pred * 100, class_labels[[2]])
            else
                pred <- sprintf("%.1f%% %s", 100 - pred * 100, class_labels[[1]])
        }
        else
            pred <- sprintf("%.2f", pred)
        title <- c(title, paste("Predicted:", pred))
    }
    if (!is.null(real_value)) {
        if (is.character(real_value)) { }
        else if (is.integer(real_value))
            real_value <- class_labels[[as.integer(real_value + 1)]]
        else if (probability)
            real_value <- sprintf("%.1f %%", real_value * 100)
        else
            real_value <- sprintf("%.2f", real_value)
        title <- c(title, paste("Real:", real_value))
    }
    if (length(title) > 1)
        do.call(paste, title)
    else
        "SLISE Explanation"
}
