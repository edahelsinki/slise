## --------------------------------------------------
## Experiment showing the robustness of different algorithms
##
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_robustness2.R [index]
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        Run it again without an index to compile a plot.
##
## Running locally:
##
##        Rscript --vanilla experiments/regression/exp_robustness2.R 1:10
##        Rscript --vanilla experiments/regression/exp_robustness2.R
##
## --------------------------------------------------

suppressMessages(source("experiments/regression/exp_robustness.R"))


plot_vertical <- function(df, metric = "mae") {
    methods <- get_regression_methods()
    df$value <- df[metric][[1]]
    df <- df %>%
        filter(type == "Vertical Outliers") %>%
        group_by(noise, method, dataset) %>%
        summarise(value = mean(value))
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
            set.seed(42 + index)

            n <- 10 * as.integer(100 * sqrt(index))
            d <- as.integer(20 * sqrt(index))
            data <- robust_data(data_create(n, d, extra_models = 0))
            cat("[robust_synth] Init:", index, "\n")

            df <- robustness_calculate(data)

            save_results(df, "robust_synth", index)
            cat("[robust_synth] Done:", index, "\n")
        }
    } else {
        df <- load_results("robust_synth")
        plot_pdf(plot_robustness(df, "mae"), "robustness_synth", 1.0, 0.5)
    }
}