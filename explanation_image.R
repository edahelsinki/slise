# This script contains helper functions for plotting explanations for images

library(ggplot2)
require(grid)
require(gridExtra)
require(reshape2)


slise_expl_color_bw <- function() c("white", "black")
slise_expl_color_rg <- function() c("red2", "green3")
slise_expl_color_rg_cb <- function() c("#d01c8b", "#4dac26")
slise_expl_color_cb <- function() c("#e66101", "#5e3c99")

# Scale the color intensity to increase visibility when printed
slise_expl_scale_colors <- function(x) {
    x <- x / max(abs(x))
    sigmoid(x * 4) * 2 - 1
}

# Plot a single image with optional outline
slise_expl_image <- function(img, contour = NULL, width = 28, height = 28,
        colors = slise_expl_color_cb(), class_labels = NULL, ..., scale_colors = TRUE) {
    image <- melt(matrix(img, height, width))
    if (scale_colors)
        image$value <- slise_expl_scale_colors(image$value)
    limits2 <- c(-1, 1) * max(abs(image$value))
    plt <- ggplot(image, aes(Var2, Var1)) + geom_raster(aes(fill = value), interpolate = FALSE)
    if (is.null(class_labels))
        plt <- plt + scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]], name = NULL)
    else
        plt <- plt + scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]],
            limits = limits2, labels = class_labels, breaks = limits2, guide = guide_legend(title = NULL))
    if (!is.null(contour)) {
        contour <- melt(matrix(contour, height, width))
        plt <- plt + stat_contour(aes(x = Var2, y = Var1, z = value), data = contour, col = "black", bins = 1)
    }
    slise_expl_theme_image(plt, ...)
}

# Plot a lineup of images with optional outlines
slise_expl_lineup <- function(imgs, labels, contours = NULL, width = 28, height = 28,
        colors = slise_expl_color_cb(), class_labels = NULL, ..., nrow = 3, scale_colors = TRUE) {
    stopifnot(length(labels) == nrow(imgs))
    labels <- factor(labels, levels = labels)
    images <- do.call(rbind, lapply(seq_along(labels), function(i) {
        image <- melt(matrix(imgs[i,], width, height))
        if (scale_colors)
            image$value <- slise_expl_scale_colors(image$value)
        image$label <- labels[[i]]
        if (!is.null(contours)) {
            cim <- melt(contours[i,], width, height)
            image$contour <- cim$value
        }
        image
    }))
    limits2 <- c(-1, 1) * max(abs(images$value))
    plt_img <- ggplot(images, aes(Var2, Var1)) + geom_raster(aes(fill = value), interpolate = FALSE) +
        facet_wrap(vars(label), scales = "free", nrow = nrow)
    if (is.null(class_labels))
        plt_img <- plt_img + scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]], name = NULL)
    else
        plt_img <- plt_img + scale_fill_gradient2(low = colors[[1]], mid = "white", high = colors[[2]],
            limits = limits2, labels = class_labels, breaks = limits2, guide = guide_legend(title = NULL))
    if (!is.null(contours)) {
        plt_img <- plt_img + stat_contour(aes(z = contour), col = "black", bins = 1)
    }
    slise_expl_theme_image(plt_img, ...) + theme(strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"))
}

# Plot a scatterplot of images
slise_expl_scatter <- function(slise, width = 28, height = 28, lineup = NULL, ..., scatter_size = 0.03, num_scatter = 100, logits = FALSE) {
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
    selected <- slise_expl_select_overlap(Y_black_box, Y_slise, scatter_size, scatter_size, sample(which(mask)), num_scatter)

    plt <- ggplot() +
        geom_tile(aes(scaled_y, scaled_y, width = scatter_size / 3, height = scatter_size / 3, fill = "Explained")) +
        lapply(selected, function(i) {
            im <- matrix(X_mask[i,], width, height)
            g <- rasterGrob(im, name=i)
            annotation_custom(g, xmin = Y_slise[[i]] - scatter_size, xmax = Y_slise[[i]] + scatter_size,
                ymin = Y_black_box[[i]] - scatter_size, ymax = Y_black_box[[i]] + scatter_size)
        }) +
        #geom_density2d(aes(Y_slise, Y_black_box, col="Dense Areas"), bins=5, linetype=3, size = 0.8) +
        geom_abline(aes(intercept = 0, slope = 1, col = "Subset"), size = 1) +
        geom_abline(aes(intercept = slise$epsilon, slope = 1, col = "Subset"), linetype = "dashed", size = 1) +
        geom_abline(aes(intercept = -slise$epsilon, slope = 1, col = "Subset"), linetype = "dashed", size = 1) +
        theme_light() + theme(aspect.ratio = 1) +
        scale_fill_manual(guide = guide_legend(title = NULL), values = c("Explained" = "black")) +
        scale_color_manual(guide = guide_legend(title = NULL),
            breaks = c("Subset", "In Lineup", "Dense Areas"),
            values = c("Subset" = "#1b9e77", "In Lineup" = "#d95f02", "Dense Areas" = "#7570b3"))

    if (!is.null(lineup)) {
        sels <- lineup$selection[lineup$selection != -1]
        plt <- plt +
            geom_tile(aes(Y_slise[sels], Y_black_box[sels], width = scatter_size * 2, height = scatter_size * 2, col = "In Lineup"), fill = "white", size = 1) +
            lapply(sels, function(i) {
                im <- matrix(X_mask[i,], width, height)
                g <- rasterGrob(im, name=-i)
                annotation_custom(g, xmin = Y_slise[[i]] - scatter_size, xmax = Y_slise[[i]] + scatter_size,
                    ymin = Y_black_box[[i]] - scatter_size, ymax = Y_black_box[[i]] + scatter_size)
            })
        if (length(sels) < length(lineup$selection)) {
            plt <- plt + geom_tile(aes(scaled_y, scaled_y, width = scatter_size * 2.4, height = scatter_size * 2.4, col = "In Lineup"), fill = "white", size = 1)
        }
    }

    plt <- plt + lapply(1, function(i) {
        im <- matrix(slise$x * 0.5 + 0.5, width, height)
        g <- rasterGrob(im, name=0)
        annotation_custom(g, xmin = scaled_y - scatter_size * 1.2, xmax = scaled_y + scatter_size * 1.2,
            ymin = scaled_y - scatter_size * 1.2, ymax = scaled_y + scatter_size * 1.2)
    })

    if (!is.null(slise$logit) && slise$logit) {
        if (logits) {
            plt + xlab("SLISE Logits") + ylab("Classifier Logits") +
                scale_x_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(logit(slise$scaled$unscale_y(x)), 2)) +
                scale_y_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(logit(slise$scaled$unscale_y(x)), 2))
        } else {
            plt + xlab("SLISE Approximation") + ylab("Classifier Prediction") +
                scale_x_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(slise$scaled$unscale_y(x), 2)) +
                scale_y_continuous(breaks = seq(-0.5, 0.5, 0.2), labels = function(x) round(slise$scaled$unscale_y(x), 2))
        }
    } else {
        plt + xlab("SLISE Approximation") + ylab("Classifier Prediction") +
            scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1))
    }
}

# Add theming to image plots
slise_expl_theme_image <- function(plt, rotate = FALSE, flip_x = FALSE, flip_y = TRUE, aspect = 1, legend = "none") {
    plt <- plt + theme_light() +
        theme(legend.position = legend,
            axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            aspect.ratio = aspect)
    if (rotate)
        plt <- plt + coord_flip()
    if (flip_x)
        plt <- plt + scale_x_reverse(expand = c(0, 0))
    else
        plt <- plt + scale_x_continuous(expand = c(0, 0))
    if (flip_y)
        plt <- plt + scale_y_reverse(expand = c(0, 0))
    else
        plt <- plt + scale_y_continuous(expand = c(0, 0))
    plt
}

# Create the subset lineup
slise_expl_get_lineup <- function(slise, num_examples = 6, include_explained = TRUE, logits = FALSE) {
    inter <- 1 / num_examples
    if (logits) {
        ys <- quantile(slise$scaled$Y[slise$subset], c(inter / 2, 1 - inter / 2))
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
slise_expl_select_overlap <- function(x, y, w, h, o, num = 50) {
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