## --------------------------------------------------
## This experiment is trying to show the optimality
##  of the different algorithms
##
##
## Usage:
##
##        Rscript --vanilla experiments/exp_optimality.R
##
## --------------------------------------------------

suppressMessages(source("experiments/utils.R"))

optimality_calculate <- function(X, Y, methods=METHODS, epsilon=0.1, lambda=0, ..., eps = seq(0.025, 0.25, 0.005)) {
    slises <- lapply(methods, function(m) {
        regression(method=m, X=X, Y=Y, epsilon=epsilon, lambda=lambda, ...)
    })
    reference <- which(methods == "lasso")
    scaled <- slises[[1]]$scaled
    out <- list()
    do.call(rbind, lapply(eps, function(e) {
        losses <- sapply(slises, function(s) loss_sharp(s$alpha, X=scaled$X, Y=scaled$Y, epsilon=e, lambda=lambda))
        losses <- losses / losses[[reference]]
        data.frame(method=methods, loss=losses, epsilon=rep(e, length(methods)))
    }))
}

optimality_plot <- function(df, epsilon=0.1, show=TRUE, title="Optimality") {
    mid <- df[df$epsilon == epsilon, ]
    labels <- rev(mid$method[order(mid$loss)])
    labelf <- sapply(labels, method_to_label, latex=FALSE)
    plt <- ggplot(df, aes(x=epsilon, y=loss, col=method)) + geom_line(position=position_dodge(0.005)) +
        ylab("Negative Loss (proportional to loss of Lasso)") + ggtitle(title) + theme_bw() +
        scale_color_brewer("Algorithm", palette="Dark2", breaks=labels, labels=labelf)
    if (show) {
        X11()
        plot(plt)
    }
    invisible(plt)
}


# This is only run when called from Rscript
if (sys.nframe() == 0L) {
    set.seed(42)
    data <- data_create(1000, 30, 5)
    df <- optimality_calculate(data$X, data$Y, scale=TRUE, lambda = 1e-6)
    saveRDS(df, file = "results/optimality.rds", compress = "xz")
    # Plot results
    optimality_plot(df, show=TRUE, title="Optimality with Synthetic data (1000 x 30) at epsilon=0.1")
}
