## --------------------------------------------------
## Experiment for use in the introduction
##
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_intro.R
##
## --------------------------------------------------

library(ggplot2)
source("experiments/regression/utils.R")

## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
# if (sys.nframe() == 0L) {
    set.seed(42)

    n <- 50
    corridor <- 0.6

    x <- seq(-1, 1, length.out = n)
    y <- -x + rnorm(n, 0, 0.15)
    x <- c(x, rep(seq(1.6, 1.8, 0.1), 2))
    y <- c(y, rep(c(1.8, 1.95), each = 3))

    a <- lm(y ~ x)$coefficients
    b <- slise.fit(x, y, epsilon = corridor)$coefficients
    names <- factor(c("OLS", "SLISE"))

    px <- c(-10, 10, 10, -10)
    py <- c(px * b[2] + b[1] + c(-1, -1, 1, 1) * corridor)

    gg <- ggplot() + theme_bw() + xlab("x") + ylab("y") +
        coord_cartesian(xlim = c(min(x), max(x)), ylim = c(min(y), max(y))) +
        theme_paper() + theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = 0.5)
        ) +
        geom_polygon(aes(x = px, y = py), fill = "#998ec333") +
        geom_point(aes(x = x, y = y), size = 2) +
        geom_abline(aes(intercept = c(a[1], b[1]), slope = c(a[2], b[2]), color = names, linetype = names), size = 1) +
        scale_color_manual(values = c("#1b9e77", "#ed9411")) +
        scale_linetype_manual(values = c("dashed", "solid"))

    plot_pdf(gg, "rr_example", 0.45, 0.3)
# }
