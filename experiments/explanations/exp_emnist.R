## --------------------------------------------------
## Experiments that use SLISE for explaining EMNIST
##
##
## Usage:
##
##  Rscript --vanilla experiments/explanations/exp_emnist.R
##
## --------------------------------------------------

source("experiments/explanations/utils.R")
source("experiments/explanations/data.R")
library(grid)
library(keras)

# The main EMNIST explanation
exp_primary <- function(dir = "experiments/results") {
    set.seed(42)
    cat("__EMNIST Primary__\n")
    data <- data_emnist(2)
    selected <- 9 # which(data$R == 2)[8]
    expl <- slise.explain(data$X, data$Y, epsilon = data$epsilon, x = selected, lambda1 = data$lambda1, lambda2 = data$lambda2, logit = TRUE)
    cairo_pdf(file.path(dir, "explanation_emnist_digit.pdf"), 0.9 * 9, 0.35 * 9)
    grid.draw(plot(expl, type = "image", plots = 3, labels = c("not 2", "2")))
    dev.off()
    cairo_pdf(file.path(dir, "explanation_emnist_dist.pdf"), 0.6 * 9, 0.35 * 9)
    plot(plot(expl, "pred", approximation = FALSE, labels = c("Class Probability", "Number of Items"), title = "", partial = TRUE) + theme_paper())
    dev.off()
    # Lineup
    cat("__EMNIST Lineup__\n")
    data2 <- data_emnist(balance = FALSE)
    Y2 <- data2$X %*% expl$alpha[-1] + expl$alpha[1]
    sub2 <- abs(Y2 - limited_logit(data2$Y)) < expl$epsilon
    lineup <- get_lineup(data2$Y, data2$R, sub2 & data2$R != 1, 6, 0.73)
    # lineup <- c(1009, 32989, 25883, 20452, 7710, 35924)
    cairo_pdf(file.path(dir, "explanation_emnist_lineup.pdf"), 1.0 * 9, 0.2 * 9)
    grid.draw(plot_mnist(
        array(t(expl$alpha[-1])[rep(1, length(lineup)), ], c(length(lineup), 28, 28)),
        array(data2$X[lineup, ], c(length(lineup), 28, 28)),
        c("not 2", "2")
    ) + facet_wrap(
        vars(Var1),
        nrow = 1,
        labeller = function(x) data.frame(Var1 = sprintf("p = %.3f", data2$Y[lineup]))
    ) + theme_image())
    dev.off()
    # Scatter
    cat("__EMNIST Scatter__\n")
    cairo_pdf(file.path(dir, "explanation_emnist_scatter.pdf"), 0.6 * 9, 0.45 * 9)
    plot(plot_scatter(expl, data2, lineup, scatter_size = 0.25))
    dev.off()
}

# Try different parameter values in a grid
exp_parameters <- function(dir = "experiments/results") {
    set.seed(42)
    cat("__EMNIST Grid__\n")
    pars <- expand.grid(
        epsilon = c(0.25, 0.5, 0.75, 1.0),
        lambda = c(0.0, 1.0, 2.0, 5.0, 10.0, 20.0)
    )
    data <- data_emnist(2)
    selected <- 9 # which(data$R == 2)[8]
    expls <- mapply(function(e, l) {
        cat(sprintf("  epsilon = %.2f  lambda = %.1f\n", e, l))
        slise.explain(data$X, data$Y, e, selected, lambda1 = l, lambda2 = l * 2, logit = TRUE)
    }, pars$epsilon, pars$lambda, SIMPLIFY = FALSE)
    models <- do.call(rbind, lapply(expls, function(e) e$alpha[-1]))
    models <- sweep(models, 1, apply(abs(models), 1, max), `/`)
    images <- do.call(rbind, lapply(expls, function(e) e$x))
    sizes <- sapply(expls, function(e) mean(e$subset))
    plt <- plot_mnist(
        array(models, c(nrow(models), 28, 28)),
        array(images, c(nrow(images), 28, 28)),
        c("not 2", "2")
    ) + facet_grid(
        vars(pars$epsilon),
        vars(pars$lambda),
        switch = "y",
        labeller = function(l) {
            if (names(l) == "pars$lambda") {
                list(paste("\u03BB =", l[[1]]))
            } else {
                list(paste("\u03B5 =", l[[1]]))
            }
        }
    ) + theme_image() +
        geom_text(aes(14.5, 29.2, label = sprintf("|S| = %.2f", sizes)), data = pars) +
        theme(panel.spacing.y = unit(0.4, "cm")) +
        coord_cartesian(clip = "off")
    cairo_pdf(file.path(dir, "explanation_emnist_parameters.pdf"), 0.9 * 9, 0.6 * 9)
    plot(plt)
    dev.off()
}

exp_investigate <- function(dir = "experiments/results") {
    set.seed(42)
    cat("__EMNIST 2-3__\n")
    data <- data_emnist(2, balance = FALSE)
    data$mask <- sample(which(data$R == 2 | data$R == 3))
    data$R <- data$R[data$mask]
    data$Y <- data$Y[data$mask]
    data$X <- data$X[data$mask, ]
    selected <- which(data$mask == 27673) # Same as the 9:th from above
    expl <- slise.explain(data$X, data$Y, epsilon = data$epsilon, x = selected, lambda1 = data$lambda, lambda2 = data$lambda2, logit = TRUE)
    cairo_pdf(file.path(dir, "explanation_emnist_other.pdf"), 0.9 * 9, 0.35 * 9)
    plot(expl, type = "image", plots = 3, labels = c("3", "2")) + theme_image()
    dev.off()
}

# Create the subset lineup
get_lineup <- function(Y, R, subset, num_examples = 6, blend = 0.5) {
    inter <- 1 / num_examples
    ys <- seq_len(num_examples) * inter - inter / 2
    ys <- (ys * (max(Y) - min(Y)) + min(Y)) * blend +
        quantile(Y[subset], ys) * (1 - blend)
    selected <- c()
    for (y in rev(ys)) {
        sel <- which.min(abs(Y - y) - subset * 1000)
        subset <- subset & R != R[sel]
        selected <- c(sel, selected)
    }
    selected
}

# Randomly select samples for the scatter, but avoid overlapping
# x,y: vector
# r: radius
# num: amount
select_nonoverlap <- function(x, y, r, num = 50) {
    r <- r * r
    sel <- c()
    for (i in seq_along(x)) {
        add <- TRUE
        for (j in sel) {
            if ((x[i] - x[j])^2 + (y[i] - y[j])^2 < r) {
                add <- FALSE
                break()
            }
        }
        if (add) {
            sel <- c(sel, i)
            if (length(sel) >= num) {
                break()
            }
        }
    }
    sel
}

# Plot a scatterplot of images
plot_scatter <- function(slise, data = NULL, lineup = NULL, width = 28, height = 28, scatter_size = 0.2, num_scatter = 100, logits = FALSE) {
    x <- limited_logit(predict(slise, slise$X))
    y <- slise$Y
    images <- slise$X
    selected <- select_nonoverlap(x, y, scatter_size, num_scatter)

    plt <- ggplot() +
        geom_point(aes(x[selected], y[selected])) +
        lapply(selected, function(i) {
            im <- matrix(images[i, ], width, height)
            g <- rasterGrob(1 - im, name = i)
            annotation_custom(g,
                xmin = x[[i]] - scatter_size, xmax = x[[i]] + scatter_size,
                ymin = y[[i]] - scatter_size, ymax = y[[i]] + scatter_size
            )
        }) +
        geom_abline(aes(intercept = 0, slope = 1, col = "Subset"), size = 1) +
        geom_abline(aes(intercept = slise$epsilon, slope = 1, col = "Subset"), linetype = "dashed", size = 1) +
        geom_abline(aes(intercept = -slise$epsilon, slope = 1, col = "Subset"), linetype = "dashed", size = 1) +
        theme_paper() +
        theme(aspect.ratio = 1) +
        scale_color_manual(
            guide = guide_legend(title = NULL),
            breaks = c("Subset", "Explained", "In Lineup"),
            values = c("Subset" = SLISE_PURPLE, "Explained" = SLISE_ORANGE, "In Lineup" = "#1b9e77")
        )

    if (!is.null(lineup)) {
        x2 <- limited_logit(predict(slise, data$X))
        y2 <- limited_logit(data$Y)
        images2 <- data$X
        sels <- lineup[lineup != -1]
        plt <- plt +
            lapply(sels, function(i) {
                im <- matrix(images2[i, ], width, height)
                g <- rasterGrob(1 - im, name = -i)
                annotation_custom(g,
                    xmin = x2[[i]] - scatter_size, xmax = x2[[i]] + scatter_size,
                    ymin = y2[[i]] - scatter_size, ymax = y2[[i]] + scatter_size
                )
            }) +
            geom_tile(aes(x2[sels], y2[sels], width = scatter_size * 2, height = scatter_size * 2, col = "In Lineup"), fill = NA, size = 1)
    }

    plt <- plt + lapply(1, function(i) {
        im <- matrix(slise$x, width, height)
        g <- rasterGrob(1 - im, name = 0)
        annotation_custom(g,
            xmin = slise$y - scatter_size, xmax = slise$y + scatter_size,
            ymin = slise$y - scatter_size, ymax = slise$y + scatter_size
        )
    }) +
        geom_tile(aes(slise$y, slise$y, width = scatter_size * 2, height = scatter_size * 2, col = "Explained"), fill = NA, size = 1)

    lims <- quantile(c(x, y), c(0.01, 0.99))
    plt + xlab("SLISE Approximation") + ylab("Classifier Prediction") +
        coord_cartesian(xlim = lims, ylim = lims) +
        scale_x_continuous(labels = function(x) round(sigmoid(x), 2)) +
        scale_y_continuous(labels = function(x) round(sigmoid(x), 2))
}

exp_new_image <- function(dir = "experiments/results") {
    cat("__EMNIST New image__\n")
    set.seed(42)
    digit <- 2
    # new_image <- matrix(1 - png::readPNG("experiments/data/new2.png")[, , 1], 1, 784)
    new_image <- matrix(c(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ), 1, 784)
    data <- data_emnist(digit, pred_fn = TRUE)
    pred <- data$pred_fn(new_image)
    expl <- slise.explain(
        data$X,
        data$Y,
        epsilon = data$epsilon,
        x = new_image,
        y = pred[2],
        lambda1 = data$lambda1,
        lambda2 = data$lambda2,
        logit = TRUE
    )
    cairo_pdf(file.path(dir, "explanation_emnist_new.pdf"), 0.9 * 9, 0.35 * 9)
    grid.draw(plot(
        expl,
        "mnist",
        plots = 3,
        title = sprintf("Predicted %.1f%% likely to be a %s", pred[2] * 100, digit),
        labels = paste0(c("not ", ""), digit)
    ))
    dev.off()
    # Get all the probabilities
    model <- keras::load_model_hdf5(file.path("experiments", "data", "emnist_model.hdf5"))
    y <- predict(model, new_image)
    colnames(y) <- 0:9
    cat("Prediction:\n")
    print(y)
}


if (sys.nframe() == 0L) { # Only run with Rscript
    exp_primary()
    # exp_parameters()
    # exp_investigate()
    exp_new_image()
}