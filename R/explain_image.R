# This script contains helper functions for plotting explanations for images


explain_slise_color_bw <- function() c("white", "black")
explain_slise_color_rg <- function() c("red2", "green3")
explain_slise_color_rg_cb <- function() c("#d01c8b", "#4dac26")
explain_slise_color_cb <- function() c("#e66101", "#5e3c99")

# Scale the color intensity to increase visibility when printed
explain_slise_scale_colors <- function(x) {
    x <- x / max(abs(x))
    sigmoid(x * 4) * 2 - 1
}

# Plot a single image with optional outline
explain_img_slise_image <- function(img, contour = NULL, width = 28, height = 28,
        colors = explain_slise_color_cb(), class_labels = NULL, ..., scale_colors = TRUE) {
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("Package \"reshape2\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    image <- reshape2::melt(matrix(img, height, width))
    if (scale_colors)
        image$value <- explain_slise_scale_colors(image$value)
    limits2 <- c(-1, 1) * max(abs(image$value))
    plt <- ggplot2::ggplot(image, ggplot2::aes(image$Var2, image$Var1)) + ggplot2::geom_raster(ggplot2::aes(fill = image$value), interpolate = FALSE)
    if (is.null(class_labels))
        plt <- plt + ggplot2::scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]], name = NULL)
    else
        plt <- plt + ggplot2::scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]],
            limits = limits2, labels = class_labels, breaks = limits2, guide = ggplot2::guide_legend(title = NULL))
    if (!is.null(contour)) {
        contour <- reshape2::melt(matrix(contour, height, width))
        plt <- plt + ggplot2::stat_contour(ggplot2::aes(x = contour$Var2, y = contour$Var1, z = contour$value), data = contour, col = "black", bins = 1)
    }
    explain_img_slise_theme(plt, ...)
}

# Plot a lineup of images with optional outlines
explain_img_slise_lineup <- function(imgs, labels, contours = NULL, width = 28, height = 28,
        colors = explain_slise_color_cb(), class_labels = NULL, ..., nrow = 3, scale_colors = TRUE) {
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("Package \"reshape2\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    stopifnot(length(labels) == nrow(imgs))
    labels <- factor(labels, levels = labels)
    images <- do.call(rbind, lapply(seq_along(labels), function(i) {
        image <- reshape2::melt(matrix(imgs[i,], width, height))
        if (scale_colors)
            image$value <- explain_slise_scale_colors(image$value)
        image$label <- labels[[i]]
        if (!is.null(contours)) {
            cim <- reshape2::melt(contours[i,], width, height)
            image$contour <- cim$value
        }
        image
    }))
    limits2 <- c(-1, 1) * max(abs(images$value))
    plt_img <- ggplot2::ggplot(images, ggplot2::aes(images$Var2, images$Var1)) + ggplot2::geom_raster(ggplot2::aes(fill = images$value), interpolate = FALSE) +
        ggplot2::facet_wrap(ggplot2::vars(images$label), scales = "free", nrow = nrow)
    if (is.null(class_labels))
        plt_img <- plt_img + ggplot2::scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]], name = NULL)
    else
        plt_img <- plt_img + ggplot2::scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]],
            limits = limits2, labels = class_labels, breaks = limits2, guide = ggplot2::guide_legend(title = NULL))
    if (!is.null(contours)) {
        plt_img <- plt_img + ggplot2::stat_contour(ggplot2::aes(z = contours), col = "black", bins = 1)
    }
    explain_img_slise_theme(plt_img, ...) + ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"), strip.text = ggplot2::element_text(color = "black"))
}

# Plot a scatterplot of images
explain_img_slise_scatter <- function(slise, width = 28, height = 28, lineup = NULL, ..., scatter_size = 0.03, num_scatter = 100, logits = FALSE) {
    if (!requireNamespace("grid", quietly = TRUE)) {
        stop("Package \"grid\" needed for the function to work. Please install it.",
        call. = FALSE)
    }
    if (!is.null(slise$logit) && slise$logit) {
        Y_black_box <- slise$scaled$Y
        Y_slise <- slise$scaled$X %*% slise$alpha[-1] + slise$alpha[[1]]
        scaled_y <- slise$scaled$scale_y(slise$y)
        mask <- Y_slise >= -0.55 & Y_slise <= 0.55
    } else {
        Y_black_box <- slise$Y
        Y_slise <- slise$scaled$unscale_y(slise$scaled$X %*% slise$alpha[-1] + slise$alpha[[1]])
        scaled_y <- slise$y
        mask <- Y_slise >= -0.05 & Y_slise <= 1.05
    }
    X_mask <- -slise$X * 0.5 + 0.5
    selected <- explain_slise_select_overlap(Y_black_box, Y_slise, scatter_size, scatter_size, sample(which(mask)), num_scatter)

    plt <- ggplot2::ggplot() +
        ggplot2::geom_tile(ggplot2::aes(scaled_y, scaled_y, width = scatter_size / 3, height = scatter_size / 3, fill = "Explained")) +
        lapply(selected, function(i) {
            im <- matrix(X_mask[i,], width, height)
            g <- grid::rasterGrob(im, name=i)
            ggplot2::annotation_custom(g, xmin = Y_slise[[i]] - scatter_size, xmax = Y_slise[[i]] + scatter_size,
                ymin = Y_black_box[[i]] - scatter_size, ymax = Y_black_box[[i]] + scatter_size)
        }) +
        #geom_density2d(ggplot2::aes(Y_slise, Y_black_box, col="Dense Areas"), bins=5, linetype=3, size = 0.8) +
        ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1, col = "Subset"), size = 1) +
        ggplot2::geom_abline(ggplot2::aes(intercept = slise$epsilon, slope = 1, col = "Subset"), linetype = "dashed", size = 1) +
        ggplot2::geom_abline(ggplot2::aes(intercept = -slise$epsilon, slope = 1, col = "Subset"), linetype = "dashed", size = 1) +
        ggplot2::theme_light() + ggplot2::theme(aspect.ratio = 1) +
        ggplot2::scale_fill_manual(guide = ggplot2::guide_legend(title = NULL), values = c("Explained" = "black")) +
        ggplot2::scale_color_manual(guide = ggplot2::guide_legend(title = NULL),
            breaks = c("Subset", "In Lineup", "Dense Areas"),
            values = c("Subset" = "#1b9e77", "In Lineup" = "#d95f02", "Dense Areas" = "#7570b3"))

    if (!is.null(lineup)) {
        sels <- lineup$selection[lineup$selection != -1]
        plt <- plt +
            ggplot2::geom_tile(ggplot2::aes(Y_slise[sels], Y_black_box[sels], width = scatter_size * 2, height = scatter_size * 2, col = "In Lineup"), fill = "white", size = 1) +
            lapply(sels, function(i) {
                im <- matrix(X_mask[i,], width, height)
                g <- grid::rasterGrob(im, name=-i)
                ggplot2::annotation_custom(g, xmin = Y_slise[[i]] - scatter_size, xmax = Y_slise[[i]] + scatter_size,
                    ymin = Y_black_box[[i]] - scatter_size, ymax = Y_black_box[[i]] + scatter_size)
            })
        if (length(sels) < length(lineup$selection)) {
            plt <- plt + ggplot2::geom_tile(ggplot2::aes(scaled_y, scaled_y, width = scatter_size * 2.4, height = scatter_size * 2.4, col = "In Lineup"), fill = "white", size = 1)
        }
    }

    plt <- plt + lapply(1, function(i) {
        im <- matrix(slise$x * 0.5 + 0.5, width, height)
        g <- grid::rasterGrob(im, name=0)
        ggplot2::annotation_custom(g, xmin = scaled_y - scatter_size * 1.2, xmax = scaled_y + scatter_size * 1.2,
            ymin = scaled_y - scatter_size * 1.2, ymax = scaled_y + scatter_size * 1.2)
    })

    if (!is.null(slise$logit) && slise$logit) {
        if (logits) {
            plt + ggplot2::xlab("SLISE Logits") + ggplot2::ylab("Classifier Logits") +
                ggplot2::scale_x_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(logit(slise$scaled$unscale_y(x)), 2)) +
                ggplot2::scale_y_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(logit(slise$scaled$unscale_y(x)), 2))
        } else {
            plt + ggplot2::xlab("SLISE Approximation") + ggplot2::ylab("Classifier Prediction") +
                ggplot2::scale_x_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(slise$scaled$unscale_y(x), 2)) +
                ggplot2::scale_y_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(slise$scaled$unscale_y(x), 2))
        }
    } else {
        plt + ggplot2::xlab("SLISE Approximation") + ggplot2::ylab("Classifier Prediction") +
            ggplot2::scale_x_continuous(limits = c(0, 1)) + ggplot2::scale_y_continuous(limits = c(0, 1))
    }
}

# Add theming to image plots
explain_img_slise_theme <- function(plt, rotate = FALSE, flip_x = FALSE, flip_y = TRUE, aspect = 1, legend = "none") {
    plt <- plt + ggplot2::theme_light() +
        ggplot2::theme(legend.position = legend,
            axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
            aspect.ratio = aspect)
    if (rotate)
        plt <- plt + ggplot2::coord_flip()
    if (flip_x)
        plt <- plt + ggplot2::scale_x_reverse(expand = c(0, 0))
    else
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0))
    if (flip_y)
        plt <- plt + ggplot2::scale_y_reverse(expand = c(0, 0))
    else
        plt <- plt + ggplot2::scale_y_continuous(expand = c(0, 0))
    plt
}

# Create the subset lineup
explain_slise_get_lineup <- function(slise, num_examples = 6, include_explained = TRUE, logits = FALSE) {
    inter <- 1 / num_examples
    if (logits) {
        ys <- stats::quantile(slise$scaled$Y[slise$subset], c(inter / 2, 1 - inter / 2))
        ys <- seq(ys[1], ys[2], (ys[2] - ys[1]) / (num_examples - 1))
        sels <- which(slise$subset)[sapply(ys, function(y) which.min(abs(slise$scaled$Y[slise$subset] - y)))]
    } else {
        ys <- seq(inter / 2, 1 - inter / 2, inter)
        sels <- which(slise$subset)[sapply(ys, function(y) which.min(abs(slise$Y[slise$subset] - y)))]
    }
    imgs <- slise$X[sels,]
    probs <- slise$Y[sels]
    if (include_explained) {
        close <- which.min((ys - slise$scaled$scale_y(slise$y)) ^ 2)
        imgs[close,] <- slise$x
        probs[close] <- slise$y
        sels[close] <- -1
    }
    probs <- round(probs, 3)
    list(probabilities = probs, images = imgs, selection = sels)
}

# Randomly select samples for the scatter, but avoid overlapping
# x,y: vector
# w,h: width,height
# o:   order (index)
# num: amount
explain_slise_select_overlap <- function(x, y, w, h, o, num = 50) {
    sel <- list()
    for (i in o) {
        add <- TRUE
        for (j in sel) {
            if (abs(x[i] - x[j]) < w && abs(y[i] - y[j]) < h) {
                add <- FALSE
                break()
            }
        }
        if (add) {
            sel[length(sel) + 1] <- i
            if (length(sel) >= num)
                break()
        }
    }
    unlist(sel)
}