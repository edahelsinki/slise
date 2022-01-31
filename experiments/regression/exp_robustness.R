## --------------------------------------------------
## Experiment showing the robustness of different algorithms
##
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_robustness.R [index]
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 17.
##        Run it again without an index to compile a plot.
##
## Running locally:
##
##        Rscript --vanilla experiments/regression/exp_robustness.R 1:17
##        Rscript --vanilla experiments/regression/exp_robustness.R
##
## --------------------------------------------------

suppressPackageStartupMessages({
    library(ggplot2)
    require(lemon)
})
source("experiments/regression/data.R")
source("experiments/regression/regression.R")
source("experiments/regression/utils.R")


robustness_calculate <- function(data,
                                 noise = seq(0.0, 0.8, 0.1),
                                 timelimit = 4000) {
    methods <- get_regression_methods()
    noise_fn <- list(
        list(fn = function(noise) outlier_data(data, noise, 0), name = "Vertical Outliers"),
        list(fn = function(noise) outlier_data(data, 0, noise), name = "Leverage Points")
    )
    names <- get_nested(methods, "name")
    df <- data.frame()
    mask <- rep(TRUE, length(methods))
    for (n in noise) {
        for (f in noise_fn) {
            data2 <- f$fn(n)
            models <- mapply(function(m, s) {
                if (s) {
                    with_timelimit(
                        m$train,
                        timelimit,
                        sprintf("%s %s %.1f %s", data$name, m$name, n, f$name),
                        data2$X,
                        data2$Y,
                        data$epsilon_sm,
                        data$lambda_sm
                    )
                } else {
                    # Skip methods that take too long
                    NA
                }
            }, methods, mask, SIMPLIFY = FALSE)
            cat(sprintf("%15s   %.1f   %s\n", data$name, n, f$name))
            flush.console()
            if (n == 0) {
                cleans <- models
            }
            mask <- sapply(models, is.rr)
            if (any(mask)) {
                diff <- mapply(function(clean, model) mean(abs(clean$coef - model$coef)), cleans[mask], models[mask])
                mae <- sapply(models[mask], function(m) mean(abs(predict(m, data$X) - data$Y)))
                mse <- sapply(models[mask], function(m) mean((predict(m, data$X) - data$Y)^2))
                slise <- sapply(models[mask], function(m) loss_sharp_res(m$coef, (predict(m, data$X) - data$Y)^2, data$epsilon_sm^2))
                df <- rbind(df, data.frame(
                    dataset = data$name,
                    noise = n,
                    diff = diff,
                    mae = mae,
                    mse = mse,
                    slise = slise,
                    method = names[mask],
                    type = f$name,
                    stringsAsFactors = TRUE
                ))
            }
        }
    }
    df$method <- factor(df$method, names, ordered = TRUE)
    df
}

plot_robustness <- function(df, metric = "mae") {
    methods <- get_regression_methods()
    df$value <- df[metric][[1]]
    df <- df %>%
        group_by(noise, method, type) %>%
        summarise(value = mean(value))
    metric <- switch(metric,
        mse = "Mean Squared Error",
        mae = "Mean Absolute Error",
        diff = "Mean Coefficient Change",
        metric
    )
    gg <- ggplot(df) +
        geom_line(aes(noise, value, linetype = method, col = method, size = method)) +
        geom_point(aes(noise, value, shape = method, col = method), size = 3) +
        facet_wrap(vars(type), nrow = 1, scales = "free") +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_size_manual(values = get_nested(methods, "size"), name = NULL) +
        # scale_y_continuous(limits = c(0, max(df$value))) +
        xlab("Fraction of outliers in the datasets") +
        ylab(metric) +
        theme_bw() +
        theme_paper() +
        theme(legend.key.width = grid::unit(2, "lines"))
    gg
}

plot_robustness2 <- function(df, metric = "mae") {
    methods <- get_regression_methods()
    df$value <- df[metric][[1]]
    df <- df %>%
        group_by(noise, method, type, dataset) %>%
        summarise(value = mean(value))
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    metric <- switch(metric,
        mse = "Mean Squared Error",
        mae = "Mean Absolute Error",
        diff = "Mean Coefficient Change",
        metric
    )
    gg <- ggplot(df) +
        geom_line(aes(noise, value, linetype = method, col = method, size = method)) +
        geom_point(aes(noise, value, shape = method, col = method)) +
        facet_wrap(vars(dataset, type), ncol = 4, scales = "free") +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_size_manual(values = get_nested(methods, "size"), name = NULL) +
        xlab("Fraction of outliers in the datasets") +
        ylab(metric) +
        theme_bw() +
        theme_paper()
    gg
}

plot_vertical <- function(df, metric = "mae") {
    methods <- get_regression_methods()
    df$value <- df[metric][[1]]
    df <- df %>%
        filter(type == "Vertical Outliers") %>%
        group_by(noise, method, dataset) %>%
        summarise(value = mean(value))
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    metric <- switch(metric,
        mse = "Mean Squared Error",
        mae = "Mean Absolute Error",
        diff = "Mean Coefficient Change",
        metric
    )
    gg <- ggplot(df) +
        geom_line(aes(noise, value, linetype = method, col = method, size = method)) +
        geom_point(aes(noise, value, shape = method, col = method)) +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_size_manual(values = get_nested(methods, "size"), name = NULL) +
        xlab("Fraction of vertical outliers in the datasets") +
        ylab(metric) +
        theme_bw() +
        theme_paper()
    reposition_legend(gg, "center", panel = "panel-4-2")
}

plot_leverage <- function(df, metric = "mae") {
    methods <- get_regression_methods()
    df$value <- df[metric][[1]]
    df <- df %>%
        filter(type == "Leverage Points") %>%
        group_by(noise, method, dataset) %>%
        summarise(value = mean(value))
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    metric <- switch(metric,
        mse = "Mean Squared Error",
        mae = "Mean Absolute Error",
        diff = "Mean Coefficient Change",
        metric
    )
    gg <- ggplot(df) +
        geom_line(aes(noise, value, linetype = method, col = method, size = method)) +
        geom_point(aes(noise, value, shape = method, col = method)) +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_size_manual(values = get_nested(methods, "size"), name = NULL) +
        xlab("Fraction of vertical outliers in the datasets") +
        ylab(metric) +
        theme_bw() +
        theme_paper()
    gg
}

## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        if (length(args) > 1) {
            reticulate::use_python(args[2])
        }
        for (index in eval(parse(text = args[1]))) {
            data <- data_at_index(index, seed_start = 642, normalise = TRUE)
            cat("[robustness] Init:", index, "\n")

            df <- robustness_calculate(data)

            save_results(df, "robustness", index)
            cat("[robustness] Done:", index, "\n")
        }
    } else {
        df <- load_results("robustness")
        # plot_pdf(plot_robustness(df, "mae"), "robustness", 1.0, 0.4)
        # plot_pdf(plot_robustness2(df, "mae"), "robustness2", 1.0, 0.8)
        plot_pdf(plot_vertical(df, "mae"), "vertical_outliers", 1.0, 0.7)
        plot_pdf(plot_leverage(df, "mae"), "leverage_points", 1.0, 0.7)
    }
}