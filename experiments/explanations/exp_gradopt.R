## --------------------------------------------------
## Example for use to describe Graduated Optimisation
##
##
## Usage:
##
##        Rscript --vanilla experiments/explanations/exp_intro.R
##
## --------------------------------------------------

library(ggplot2)
source("experiments/explanations/utils.R")

plot_gradopt <- function(filename) {
    set.seed(42)
    size <- 40

    x <- matrix(runif(size, -0.5, 0.5), size, 1)
    y <- -x + rnorm(size, 0, 0.15)

    a <- seq(-1.6, -0.2, 0.002)
    l1 <- sapply(a, loss_smooth, x, y, 0.1, 10)
    l2 <- sapply(a, loss_smooth, x, y, 0.1, 400)
    l3 <- sapply(a, loss_smooth, x, y, 0.1, 1500)
    l4 <- sapply(a, loss_sharp, x, y, 0.1)

    df <- data.frame(
        x = c(rep(a, 4)),
        y = c(l1, l2, l3, l4),
        b = c(rep(1:4, each = length(a)))
    )
    lblr <- as_labeller(function(b) {
        expression(
            beta == 0,
            beta > 0,
            paste(beta, "\u226B", 0),
            beta %->% infinity
        )
    }, default = label_parsed)
    gg <- ggplot(df) +
        xlab(expression(alpha)) +
        ylab("Loss") +
        geom_line(aes(x = x, y = y)) +
        facet_wrap(~b, ncol = 4, scale = "free_y", labeller = lblr) +
        theme_paper() +
        theme(axis.ticks = element_blank(), axis.text = element_blank())

    cairo_pdf(filename, width = 1.0 * 9, height = 0.22 * 9)
    plot(gg)
    dev.off()
}

if (sys.nframe() == 0L) { # Only run with Rscript
    plot_gradopt("experiments/results/gradopt_example.pdf")
}