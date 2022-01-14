## --------------------------------------------------
## Experiment showing the how well the algorithms work on clean data
##
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_no_outliers.R [index]
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 17*(7+1).
##        Run it again without an index to compile a plot.
##
## Running locally:
##
##        Rscript --vanilla experiments/regression/exp_no_outliers.R 1:136
##        Rscript --vanilla experiments/regression/exp_no_outliers.R
##
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
source("experiments/regression/data.R")
source("experiments/regression/regression.R")
source("experiments/regression/utils.R")


## --------------------------------------------------
## Experiment Calculations
## --------------------------------------------------
calculate <- function(method,
                      data,
                      k = 10,
                      timelimit = 1000) {
    folds <- rep(1:k, length.out = nrow(data$X))
    folds <- sample(folds)

    df <- data.frame()
    for (i in 1:k) {
        rr <- with_timelimit(
            method$train,
            timelimit,
            sprintf("%s %s", data$name, method$name),
            data$X[folds != i, ],
            data$Y[folds != i],
            data$epsilon_md,
            data$lambda_md
        )
        if (is.rr(rr) || is.rrr(rr)) {
            res <- predict(rr, data$X[folds == i, ]) - data$Y[folds == i]
            df <- rbind(
                df,
                data.frame(
                    method = method$name,
                    dataset = data$name,
                    mae = mean(abs(res)),
                    mse = mean(res^2),
                    stringsAsFactors = TRUE
                )
            )
        } else {
            break()
        }
    }
    df
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_mae <- function(df) {
    suppressPackageStartupMessages(library(ggplot2))
    methods <- get_regression_methods(add_mean = TRUE)
    df <- df %>%
        filter(dataset != "Synthetic") %>%
        group_by(dataset, method) %>%
        summarize(mae = mean(mae))
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    ggplot(df) +
        geom_point(aes(1, 0), alpha = 0) +
        theme_bw() +
        theme_paper() +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        xlab(NULL) +
        ylab("Mean Absolute Error") +
        ggrepel::geom_text_repel(
            aes(1.05, mae, label = method),
            force = 0.2,
            box.padding = 0.15,
            segment.alpha = 0.3,
            direction = "y",
            hjust = 0,
            nudge_x = 0.15
        ) +
        geom_point(aes(1, mae, col = method, shape = method), size = 3) +
        theme(
            legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        ) +
        scale_x_continuous(limits = c(0.9, 2))
}

plot_mae2 <- function(df) {
    suppressPackageStartupMessages(library(ggplot2))
    methods <- get_regression_methods(add_mean = TRUE)
    df <- df %>%
        filter(dataset != "Synthetic") %>%
        group_by(dataset, method) %>%
        summarize(mae = mean(mae))
    df2 <- df %>%
        filter(method == "Mean") %>%
        transmute(ref = mae, dataset = dataset)
    df <- df %>%
        inner_join(df2, "dataset") %>%
        mutate(mae = mae / ref)
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    ggplot(df, aes(dataset, mae, group = method, col = method, shape = method, linetype = method)) +
        theme_bw() +
        theme_paper() +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        xlab(NULL) +
        ylab("Relative Mean Absolute Error") +
        geom_point(size = 3) +
        geom_line() +
        theme(legend.key.width = grid::unit(2, "lines"))
}

print_mae <- function(df) {
    methods <- get_regression_methods(add_mean = TRUE)
    df$method <- factor(df$method, get_nested(methods, "name"), get_nested(methods, "latex"))
    levels(df$dataset) <- sapply(levels(df$dataset), latex_name)
    df <- df %>%
        filter(dataset != "Synthetic") %>%
        group_by(dataset, method) %>%
        summarize(mae = mean(mae)) %>%
        tidyr::pivot_wider(names_from = method, values_from = mae)
    tab <- print(
        xtable::xtable(
            df,
            align = c("l", "l", rep("r", ncol(df) - 1)),
            label = "tab:exp:clean",
            caption = "TODO"
        ),
        sanitize.text.function = identity,
        sanitize.colnames.function = identity,
        include.rownames = FALSE,
        comment = FALSE,
        booktabs = TRUE,
        print.results = FALSE,
        caption.placement = "top",
        table.placement = NULL
    )
    tab <- stringr::str_replace(tab, "centering", "centering\n\\\\rowcolors{2}{white}{gray!25}")
    tab <- stringr::str_replace(tab, "dataset", "")
    cat(tab)
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
            methods <- get_regression_methods(add_mean = TRUE)
            method_index <- (index - 1) %% length(methods) + 1
            data_index <- floor((index - 1) / length(methods)) + 1
            method <- methods[[method_index]]
            data <- data_at_index(data_index, seed_start = 842, normalise = TRUE)
            cat(sprintf("[no outliers] Init: %2d   %s   %s\n", index, method$name, data$name))

            df <- calculate(method, data)
            save_results(df, "no_outliers", data_index, method$tag)
            cat(sprintf("[no outliers] Done: %2d   %s   %s\n", index, method$name, data$name))
        }
    } else {
        df <- load_results("no_outliers")
        print_mae(df)
        plot_pdf(plot_mae(df), "no_outliers", 0.7, 0.5)
        plot_pdf(plot_mae2(df), "no_outliers2", 0.7, 0.4)
    }
}