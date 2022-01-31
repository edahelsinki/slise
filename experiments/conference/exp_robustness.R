## --------------------------------------------------
## Experiment showing the robustness of different algorithms
##
##
## Usage:
##
##        Rscript --vanilla experiments/exp_robustness.R
##
## --------------------------------------------------

suppressMessages(source("experiments/utils.R"))


robustness_calculate <- function(X, Y, epsilon = 0.1, lambda = 0, methods = METHODS, noise_interval=0.05, scale=TRUE, noise_fn=noise_unif, iterations=5, ...) {
    df <- do.call(rbind, lapply(seq(noise_interval / 2, 1 - noise_interval / 2, noise_interval), function(n) {
        do.call(rbind, lapply(1:iterations, function(i) {
            data <- noise_fn(X, Y, n)
            slises <- lapply(methods, function(m) regression(data$X, data$Y, m, epsilon, lambda=lambda, scale=scale, ...))
            residuals <- sapply(slises, function(s) sum(abs(X %*% s$coefficients[-1] + s$coefficients[[1]] - Y)))
            data <- data_preprocess(X, Y, intercept=TRUE, scale=scale)
            losses <- sapply(slises, function(s) loss_sharp(data$scale_alpha(s$coefficients), data$X, data$Y, epsilon, lambda))
            data.frame(noise=rep(n, length(methods)), method=methods, residual=residuals, loss=losses, iteration=rep(i, length(methods)))
        }))
    }))
    df
}

noise_unif <- function(X, Y, noise) {
    if (noise > 0) {
        Y[1:floor(length(Y) * noise)] <- runif(floor(length(Y) * noise), min(Y), max(Y))
    }
    list(X=X, Y=Y)
}

robustness_plot <- function(res, show=TRUE, values=c("loss", "residual"), labels=c("Loss", "Residuals"), title="", maxn=1, normalise=FALSE) {
    res <- res[res$noise <= maxn, ]
    df <- aggregate(res[c("loss", "residual")], list(res$method, res$noise), mean)
    methods <- unique(df$Group.1)
    labelf <- sapply(methods, method_to_label, latex=FALSE)
    invisible(lapply(seq_along(values), function(i) {
        val <- values[[i]]
        lab <- labels[[i]]
        mid <- sort(unique(df$Group.2))
        mid <- mid[[ceiling(length(mid) / 2)]]
        ord <- rev(order(df[[val]][df$Group.2 == mid]))
        if (normalise) df[[val]] <- df[[val]] / df[[val]][seq_along(methods)]
        plt <- ggplot(df, aes(x=Group.2, y=df[[val]], col=Group.1)) + geom_line(position = position_dodge(0.01)) +
            scale_color_brewer(name="Algorithm", palette="Dark2", breaks=methods[ord], labels=labelf[ord]) +
            xlab("Noise") + ylab(lab) + ggtitle(title) + theme_light() + theme(legend.position="right")
        if (show) {
            X11()
            plot(plt)
        }
        plt
    }))
}


if (sys.nframe() == 0L) {
    set.seed(42)
    jets <- data_jets(1000)
    res <- robustness_calculate(jets$X, jets$Y, iterations = 5, lambda = 1e-6)
    saveRDS(res, file = "experiments/results/robustness.rds", compress = "xz")
    robustness_plot(res, title="Robustness with Physics Data")
}
