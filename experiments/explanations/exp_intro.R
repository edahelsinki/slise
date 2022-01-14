## --------------------------------------------------
## Plots for the introduction
##
##
## Usage:
##
##  Rscript --vanilla experiments/explanations/exp_intro.R
##
## --------------------------------------------------

source("experiments/explanations/utils.R")

## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {

    # Local Linear Approximation
    x <- c(-2, runif(80, -3.3, 3.3))
    set.seed(42)
    y <- cos(x)
    expl <- slise.explain(x, y, 0.25, 1)
    cairo_pdf("experiments/results/intro_approximation.pdf", width = 0.55 * 9, height = 0.3 * 9)
    plot(plot(expl, "2d", labels = c("x", "cos(x)"), title = "", partial = TRUE, size = 1.5) + coord_fixed() + theme_paper())
    dev.off()

    # Constrained Data Manifold
    set.seed(42)
    x1 <- runif(50, -3, 2)
    x2 <- x1 * 0.55 + rnorm(50, 1, 0.15)
    x3 <- runif(50, -2, 3)
    x4 <- x3 * 0.55 - rnorm(50, 1, 0.15)
    X <- cbind(c(x1, x3), c(x2, x4))
    Y <- c(x1 < -0.75, x3 > 0.75)
    gg <- ggplot() +
        geom_point(aes(X[, 1], X[, 2], shape = Y, color = Y)) +
        geom_text(aes(c(-1.5, 1.5), c(1.5, -1.5), label = "?")) +
        scale_shape_manual(
            limits = c(FALSE, TRUE),
            values = c(0, 4),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        scale_color_manual(
            limits = c(FALSE, TRUE),
            values = c(SLISE_ORANGE, SLISE_PURPLE),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        labs(x = "x1", y = "x2") +
        theme_paper()
    cairo_pdf("experiments/results/intro_manifold.pdf", width = 0.45 * 9, height = 0.3 * 9)
    plot(gg)
    dev.off()

    # Constrained Data Manifold 2
    set.seed(42)
    num <- 30
    x1 <- seq(length.out = num, -2.5, 2.4)
    x2 <- x1 * 0.15 + 1 + rnorm(num, 0, 0.1)
    x3 <- seq(length.out = num, -2.4, 2.5)
    x4 <- x3 * 0.15 - 1 + rnorm(num, 0, 0.1)
    df <- data.frame(x = c(x1, x3), y = c(x2, x4), z = c(x1 < -0.4, x3 > 0.4))
    gd <- 50
    grid <- expand.grid(x = seq(min(df$x), max(df$x), length.out = gd), y = seq(min(df$y), max(df$y), length.out = gd))
    grid2 <- grid
    grid$z <- apply(grid, 1, function(x) {
        dist <- (df$x - x[1])^2 + (df$y - x[2])^2 / (df$z * 5 + 1)
        as.numeric(df$z[which.min(dist)])
    })
    grid$f <- "Decision Boundary 1"
    grid2$z <- apply(grid2, 1, function(x) {
        dist <- (df$x - x[1])^2 + (df$y - x[2])^2 / (4 - df$z * 3)
        as.numeric(df$z[which.min(dist)])
    })
    grid2$f <- "Decision Boundary 2"
    grid <- rbind(grid, grid2)
    gg <- ggplot(grid, aes(x, y, shape = z, color = z, z = z)) +
        geom_point(data = df) +
        geom_contour(bins = 1, color = "black", linetype=2) +
        facet_grid(cols = vars(f)) +
        scale_shape_manual(
            limits = c(FALSE, TRUE),
            values = c(4, 0),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        scale_color_manual(
            limits = c(FALSE, TRUE),
            values = c(SLISE_ORANGE, SLISE_PURPLE),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        labs(x = "x1", y = "x2") +
        coord_fixed() +
        theme_paper()
    cairo_pdf("experiments/results/intro_manifold2.pdf", width = 0.85 * 9, height = 0.3 * 9)
    plot(gg)
    dev.off()

    # Constrained Data Manifold 3
    set.seed(42)
    num <- 30
    x1 <- seq(length.out = num, -2.5, 2.4)
    x2 <- x1 * 0.15 + 1 + rnorm(num, 0, 0.1)
    x3 <- seq(length.out = num, -2.4, 2.5)
    x4 <- x3 * 0.15 - 1 + rnorm(num, 0, 0.1)
    df <- data.frame(x = c(x1, x3), y = c(x2, x4), z = c(x1 < -0.4, x3 > 0.4))
    dl <- data.frame(
        x=c(-0.37, -0.43, -2.5, 0.37, 0.43, 2.5, -0.43, -0.37, 2.5, 0.43, 0.37, -2.5),
        y=c(1.5, 0.75, -1.0, -1.5, -0.75, 1.0, 1.5, 0.9, -0.2, -1.5, -0.9, 0.2),
        z=rep(rep(factor(1:2), each=3), 2),
        f=rep(sprintf("Decision Boundary %d", 1:2), each=6)
    )
    gg <- ggplot(dl, aes(x, y, shape = z, color = z, group=z)) +
        geom_point(data = df) +
        geom_path(color="black", linetype=2) +
        facet_grid(cols = vars(f)) +
        scale_shape_manual(
            limits = c(FALSE, TRUE),
            values = c(4, 0),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        scale_color_manual(
            limits = c(FALSE, TRUE),
            values = c(SLISE_ORANGE, SLISE_PURPLE),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        labs(x = "x1", y = "x2") +
        coord_fixed() +
        theme_paper()
    cairo_pdf("experiments/results/intro_manifold3.pdf", width = 0.85 * 9, height = 0.3 * 9)
    plot(gg)
    dev.off()

    # Constrained Data Manifold 4
    set.seed(42)
    num <- 30
    x1 <- seq(length.out = num, -2.5, 2.4)
    x2 <- x1 * 0.15 + 1.3 + rnorm(num, 0, 0.1)
    x3 <- seq(length.out = num, -2.4, 2.5)
    x4 <- x3 * 0.15 - 1.3 + rnorm(num, 0, 0.1)
    df <- data.frame(x = c(x1, x3), y = c(x2, x4), z = c("A", "B")[1+c(x1 < -0.4, x3 > 0.4)])
    dl <- data.frame(
        x=c(-0.39, -0.41, -2.5, 0.39, 0.41, 2.5, -0.63, -0.37, 2.5, 0.63, 0.37, -2.5),
        y=c(1.7, 0.95, -1.0, -1.7, -0.95, 1.0, 1.7, 1.0, 0.2, -1.7, -1.0, -0.2),
        z=rep(rep(factor(1:2), each=3), 2),
        f=rep(sprintf("Decision Boundary %d", 2:1), each=6)
    )
    da1 <- data.frame(
        x = c(df$x[18]-0.06, df$x[18] - 0.6, df$x[18]-0.02, df$x[18] - 0.2),
        y=c(df$y[18]+0.01, df$y[18] + 0.05, df$y[18]-0.055, df$y[18] - 0.55),
        z="D",
        f=rep(sprintf("Decision Boundary %d", 2:1), each=2)
    )
    da2 <- data.frame(
        x = c(df$x[53]-0.06, df$x[53] - 0.6, df$x[53]-0.035, df$x[53] - 0.35),
        y = c(df$y[53]-0.01, df$y[53] - 0.1, df$y[53]+0.045, df$y[53] + 0.45),
        z = "D",
        f = rep(sprintf("Decision Boundary %d", 1:2), each=2)
    )
    gg <- ggplot(dl, aes(x, y, group=z)) +
        geom_point(aes(shape = z, color = z), data = df) +
        geom_path(color="black", linetype=2) +
        geom_path(aes(linetype=z), color="#1b9e77", data=da1, arrow=arrow(length=unit(0.08, "inch"))) +
        geom_path(aes(linetype=z), color="#1b9e77", data=da2, arrow=arrow(length=unit(0.08, "inch"))) +
        facet_grid(cols = vars(f)) +
        scale_shape_manual(
            limits = c("A", "B"),
            values = c(4, 0),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        scale_color_manual(
            limits = c("A", "B"),
            values = c(SLISE_ORANGE, SLISE_PURPLE),
            labels = c("Class A", "Class B"),
            name = NULL
        ) +
        scale_linetype_manual(
            limits = c("D"),
            values = c(1),
            labels = c("Gradient"),
            name = NULL
        ) +
        labs(x = "x1", y = "x2") +
        coord_fixed() +
        theme_paper()
    cairo_pdf("experiments/results/intro_manifold4.pdf", width = 0.9 * 9, height = 0.35 * 9)
    plot(gg)
    dev.off()
}