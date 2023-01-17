## --------------------------------------------------
## Experiments that use SLISE combined with Activation Maximisation
##
##
## Usage:
##
##        Rscript --vanilla experiments/explanations/exp_internal.R
##
## --------------------------------------------------

source("experiments/explanations/utils.R")
source("experiments/explanations/data.R")
library(grid)
library(gridExtra)


emnist_get_internal <- function(datadir = "experiments/data") {
    data <- readRDS(file.path(datadir, "emnist.rds"))
    pred <- readRDS(file.path(datadir, "emnist_preds.rds"))
    inter <- readRDS(file.path(datadir, "emnist_internal.rds"))
    inter$image <- data$image[int$selected, ]
    inter$label <- data$label[int$selected]
    inter$pred <- sapply(seq_along(int$label), function(i) pred[int$selected[i], int$label[i] + 1])
    inter
}

exp_internal <- function(dir = "experiments/results") {
    set.seed(42)
    int <- emnist_get_internal()
    emnist <- data_emnist(3)
    # plot_mnist(array(int$image[which(int$label == 3),], c(40, 28, 28)), colours = c("white", "black")) + facet_wrap(vars(Var1), ncol=7)
    selected <- which(int$label == 3)[19]
    # plot_mnist(array(am, c(32, 28, 28)), int$image[selected, ], colours = c("white", "black"), enhance_colours = FALSE) + facet_wrap(vars(Var1), ncol=7)
    expl <- slise.explain(
        int$internal[emnist$mask, ],
        emnist$Y,
        epsilon = 0.5,
        x = int$internal[int$selected[selected], ],
        y = int$pred[selected],
        logit = TRUE,
        lambda1 = 100
    )
    # plot(expl, "pred")
    # print(expl)
    nodes <- lapply(int$nodes, function(n) n[selected, ])
    cairo_pdf(file.path(dir, "internal_emnist_colour.pdf"), 1.0 * 9, 0.35 * 9)
    grid.draw(plot_internal(expl, nodes, int$image[selected, ], 6, bw = FALSE))
    dev.off()
    cairo_pdf(file.path(dir, "internal_emnist_grey.pdf"), 1.0 * 9, 0.35 * 9)
    grid.draw(plot_internal(expl, nodes, int$image[selected, ], 6, bw = TRUE))
    dev.off()
}

plot_internal <- function(slise, nodes, image = NULL, num_nodes = 6, impact = TRUE, bw = FALSE) {
    selected <- order(-abs(if (impact) slise$impact[-1] else slise$alpha[-1]))
    if (num_nodes < length(nodes)) {
        selected <- selected[1:num_nodes]
    } else {
        num_nodes <- length(nodes)
    }
    nodes <- array(t(simplify2array(nodes[selected])), c(num_nodes, 28, 28))
    labeller <- function(x) list(paste("Neuron", selected))
    node_plot <- if (bw) {
        plot_mnist(nodes - 0.5, image, enhance_colours = FALSE, colours = c("white", "black")) +
            facet_wrap(vars(Var1), labeller = labeller, nrow = 1) +
            theme_image(legend.position = "none", axis.title.y = element_text()) +
            labs(y = "\nActivation\nMaximisations")
    } else {
        plot_mnist(nodes - 0.5, image, c("White", "Black"), enhance_colours = FALSE) +
            facet_wrap(vars(Var1), labeller = labeller, nrow = 1) +
            theme_image(
                legend.position = "left",
                legend.box.margin = margin(c(0, -10, 0, 0))
            ) + # , axis.title.y = element_text()) + labs(y = "Activation\nMaximisations")
            guides(fill = guide_legend(title.hjust = 0.5, label.position = "bottom"))
    }
    df <- data.frame(
        x = slise$x[selected],
        alpha = slise$alpha[-1][selected],
        impact = slise$impact[-1][selected],
        name = factord(paste("Neuron", selected))
    )
    labels <- factord(c("Activation", "Coefficient", "Impact"))
    cols_plot <- ggplot(df) +
        geom_col(aes(labels[1], x)) +
        geom_col(aes(labels[2], alpha)) +
        geom_col(aes(labels[3], impact)) +
        coord_flip() +
        scale_x_discrete(limits = rev) +
        facet_wrap(vars(name), nrow = 1) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_paper() +
        theme(strip.text = element_blank())
    grid.arrange(node_plot, cols_plot, nrow = 2, heights = c(0.6, 0.4))
}


if (sys.nframe() == 0L) { # Only run with Rscript
    exp_internal()
}