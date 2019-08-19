## --------------------------------------------------
## Plot the results for the robust regression experiments
##
##
## Usage:
##
##        Rscript --vanilla experiments/plot_regression.R
##
## Requires results from:
##        experiments/exp_robustness.R
##        experiments/exp_optimality.R
##        experiments/exp_scalability.R
## --------------------------------------------------

## --------------------------------------------------
## Libraries
## --------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)
library(latex2exp)
source("experiments/utils.R")

DIRECTORY <- "results"

## Algorithm order
alg_list <- c("SLISE", "sparseLTS", "fastLTS", "MM Lasso", "MM Estimator", "LAD Lasso", "Lasso")
alg_line <- c(1, 2, 3, 4, 3, 4, 2)
alg_size <- c(1, rep(0.5, 6))
alg_icon <- c(46, 15, 5, 17, 6, 8, 46)


## --------------------------------------------------
## (1) Scalability
## --------------------------------------------------
plot_scalability <- function(to_pdf = FALSE) {
    ## Format of scales
    point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

    ## Read data
    res_orig <- readRDS("results/scalability.rds")

    timelimit <- 10 * 60

    ## Data frame for scalability - varying samples
    res <- res_orig[(is.na(res_orig$sparsity) | res_orig$sparsity > 1) & res_orig$d == 100, ]
    res$time[res$time > timelimit] <- NA
    sizes      <- unique(res$n)
    sizes      <- sizes[!is.na(sizes)]
    res$method <- sapply(as.character(res$method), method_to_label)
    methods    <- unique(res$method)
    df_samples <- do.call(rbind, lapply(sizes, function(n) {
                                    times <- sapply(methods, function(m) mean(res$time[res$n == n & res$method == m], na.rm=TRUE))
                                    algs <- methods
                                    data.frame(size=rep(n, length(times)), algorithm=algs, time=times, n=rep(n, length(times)), stringsAsFactors = FALSE)
                                }))

    res <- res_orig[(is.na(res_orig$sparsity) | res_orig$sparsity > 1) & res_orig$n == 10000, ]
    res$time[res$time > timelimit] <- NA
    sizes <- unique(res$d)
    sizes <- sizes[!is.na(sizes)]
    res$method <- sapply(as.character(res$method), method_to_label)
    methods    <- unique(res$method)
    df_dimensions <- do.call(rbind, lapply(sizes, function(d) {
                                        times <- sapply(methods, function(m) mean(res$time[res$d == d & res$method == m], na.rm=TRUE))
                                        algs <- methods
                                        data.frame(dimensions=rep(d, length(times)), algorithm=algs, time=times, d=rep(d, length(times)), stringsAsFactors = FALSE)}))

    df_samples$algorithm  <- ordered(df_samples$algorithm, levels = alg_list, labels = alg_list)
    df_dimensions$algorithm  <- ordered(df_dimensions$algorithm, levels = alg_list, labels = alg_list)


    ## ------------------------------
    ## Plot
    ## ------------------------------
    p <- ggplot(df_samples)

    p <- p + geom_line(aes(x = size, y = time, group = algorithm, size = algorithm, linetype = algorithm))
    p <- p + geom_point(aes(x = size, y = time, group = algorithm, shape = algorithm), size = 1.5)

    p <- p + geom_hline(yintercept = 600, linetype = "dashed", colour = "black", size = 1)

    p <- p + xlab("Number of Samples")
    p <- p + ylab("Time (s)")

    p <- p + scale_x_log10(breaks = sort(unique(df_samples$n)), minor_breaks = FALSE, labels = point)
    p <- p + scale_y_log10(breaks=c(1e0, 1e1, 1e2, 1e3, 1e4), minor_breaks = FALSE)
    p <- p + scale_size_manual(values = alg_size)
    p <- p + scale_linetype_manual(values = alg_line)
    p <- p + scale_shape_manual(values = alg_icon)

    p <- p + theme_bw()

    p <- p + theme(legend.position=c(0.2, 0.85),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "white", colour = "gray"),
                legend.key.height=unit(0.3, "line"),
                legend.key.width=unit(1.5, "line")
                )


    p <- p + theme(axis.text.x  = element_text(size = 7))
    p <- p + theme(axis.text.y  = element_text(size = 7))

    p <- p + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

    p <- p + theme(panel.border = element_blank())
    p <- p + theme(axis.line = element_line(colour = "black"))

    if (to_pdf) cairo_pdf(file.path(DIRECTORY, "scalability_samples.pdf"), width = 0.45 * 9, height = 0.45 * 9)
    plot(p)
    if (to_pdf) dev.off()


    p <- ggplot(df_dimensions)

    p <- p + geom_line(aes(x = dimensions, y = time, group = algorithm, size = algorithm, linetype = algorithm))
    p <- p + geom_point(aes(x = dimensions, y = time, group = algorithm, shape = algorithm), size = 1.5)

    p <- p + geom_hline(yintercept = 600, linetype = "dashed", colour = "black", size = 1)

    p <- p + xlab("Number of Dimensions")
    p <- p + ylab("Time (s)")

    p <- p + scale_x_log10(breaks = sort(unique(df_dimensions$d)), minor_breaks = FALSE, labels = point)
    p <- p + scale_y_log10(breaks=c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6), minor_breaks = FALSE)
    p <- p + scale_size_manual(values = alg_size)
    p <- p + scale_linetype_manual(values = alg_line)
    p <- p + scale_shape_manual(values = alg_icon)

    p <- p + theme_bw()

    p <- p + theme(legend.position=c(0.2, 0.85),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "white", colour = "gray"),
                legend.key.height=unit(0.3, "line"),
                legend.key.width=unit(1.5, "line")
                )

    p <- p + theme(axis.text.x  = element_text(size = 7))
    p <- p + theme(axis.text.y  = element_text(size = 7))

    p <- p + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

    p <- p + theme(panel.border = element_blank())
    p <- p + theme(axis.line = element_line(colour = "black"))

    if (to_pdf) cairo_pdf(file.path(DIRECTORY, "scalability_dimensions.pdf"), width = 0.45 * 9, height = 0.45 * 9)
    plot(p)
    if (to_pdf) dev.off()
}


## --------------------------------------------------
## (2) Robustness
## --------------------------------------------------
plot_robustness <- function() {
    res_rob <- readRDS("results/robustness.rds")

    res_rob$algorithm <- sapply(as.character(res_rob$method), method_to_label)
    res_rob$method    <- NULL
    res_rob <- res_rob %>% group_by(algorithm, noise) %>% summarise(mean_resid = mean(residual), mean_loss = mean(loss))
    res_rob$algorithm  <- ordered(res_rob$algorithm, levels = alg_list, labels = alg_list)

    p <- ggplot(res_rob)
    p <- p + geom_line(aes(x = noise, y = mean_resid, group = algorithm, size = algorithm, linetype = algorithm))
    p <- p + geom_point(aes(x = noise, y = mean_resid, group = algorithm, shape = algorithm), size = 1.5)

    p <- p + scale_size_manual(values = alg_size)
    p <- p + scale_linetype_manual(values = alg_line)
    p <- p + scale_shape_manual(values = alg_icon)

    p <- p + theme_bw()

    p <- p + xlab("Fraction of noise in the dataset")
    p <- p + ylab("Sum of residuals")

    p <- p + theme(legend.position=c(0.2, 0.82),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "white", colour = "gray"),
                legend.key.height=unit(0.8, "line"),
                legend.key.width=unit(1.5, "line")
                )

    p <- p + theme(panel.border = element_blank())
    p <- p + theme(axis.line = element_line(colour = "black"))

    cairo_pdf(file.path(DIRECTORY, "robustness.pdf"), width = 0.45 * 9, height = 0.45 * 9)
    plot(p)
    dev.off()
}


## --------------------------------------------------
## (3) Optimality
## --------------------------------------------------
plot_optimality <- function() {
    res_opt <- readRDS("results/optimality.rds")

    res_opt$algorithm <- sapply(as.character(res_opt$method), method_to_label)
    res_opt$method     <- NULL
    res_opt <- res_opt[res_opt$epsilon > 0.05, ]
    res_opt$algorithm  <- ordered(res_opt$algorithm, levels = alg_list, labels = alg_list)

    p <- ggplot(res_opt)
    p <- p + geom_line(aes(x = epsilon, y = loss, group = algorithm, size = algorithm, linetype = algorithm))
    p <- p + geom_point(aes(x = epsilon, y = loss, group = algorithm, shape = algorithm), size = 1.5)

    p <- p + scale_size_manual(values = alg_size)
    p <- p + scale_linetype_manual(values = alg_line)
    p <- p + scale_shape_manual(values = alg_icon)
    p <- p + coord_cartesian(ylim=c(1, max(res_opt$loss)))

    p <- p + theme_bw()

    p <- p + xlab("ε")
    p <- p + ylab("Negative Loss (proportional to the loss of Lasso)")

    p <- p + theme(legend.position=c(0.80, 0.82),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "white", colour = "gray"),
                legend.key.height=unit(0.8, "line"),
                legend.key.width=unit(1.5, "line")
                )

    p <- p + theme(panel.border = element_blank())
    p <- p + theme(axis.line = element_line(colour = "black"))

    cairo_pdf(file.path(DIRECTORY, "optimality.pdf"), width = 0.45 * 9, height = 0.45 * 9)
    plot(p)
    dev.off()
}


if (sys.nframe() == 0L) {
    plot_scalability(TRUE)
    plot_robustness()
    plot_optimality()
}
