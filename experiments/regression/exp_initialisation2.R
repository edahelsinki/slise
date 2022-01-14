## --------------------------------------------------
## Peform experiment for showing failure modes for some initialisation schemes
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/dami2021/exp_initialisation2.R
##
## --------------------------------------------------

suppressMessages(library(ggplot2))
suppressMessages(source("experiments/regression/utils.R"))

set.seed(42)

# Clean data

X <- seq(0, 10, length.out = 25) # + rnorm(25, 0, 0.1)
Y <- 3.5 - X * 0.7

# Main outliers

X <- c(X, seq(8, 10, length.out = 5))
Y <- c(Y, rep(0, 5))

# OLS outliers

X <- c(X, seq(8, 10, length.out = 3), seq(0, 2, length.out = 3))
Y <- c(Y, seq(3, 4, length.out = 3), seq(-4, -3, length.out = 3))

# Models

linear <- lm(Y ~ X)
slise <- slise.fit(X, Y, 0.2, 0.01)
slise_ols <- slise.fit(X, Y, 0.2, 0.01, initialisation = slise_initialisation_ols)
slise_lasso <- slise.fit(X, Y, 0.2, 0.01, initialisation = slise_initialisation_lasso)
slise_zero <- slise.fit(X, Y, 0.2, 0.01, initialisation = slise_initialisation_zeros)

# Plot
names <- c(
    "OLS",
    "SLISE from LASSO",
    "SLISE from OLS",
    "SLISE from ZEROS",
    "SLISE from CANDIDATES"
)
res <- as.data.frame(rbind(
    coef(linear),
    coef(slise_lasso),
    coef(slise_ols),
    coef(slise_zero),
    coef(slise)
)) %>% mutate(method = factor(names, names))
gg <- ggplot() +
    theme_bw() +
    theme_paper() +
    geom_point(aes(X, Y), size = 3, shape = 1) +
    geom_abline(
        aes(intercept = res[[1]], slope = res[[2]], col = res$method, linetype = res$method),
        size = 1
    ) +
    scale_color_manual(values = c("#1b9e77", "#8877bb", "#8877bb", "#8877bb", "#ed9411")) +
    scale_linetype_manual(values = c("dotted", "dashed", "dashed", "dashed", "solid")) +
    coord_fixed()

# Output

plot_pdf(gg, "initialisation2", 0.6, 0.3)