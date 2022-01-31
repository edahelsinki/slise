## --------------------------------------------------
## Peform experiment for Robust Regression Scalability
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_scalability.R [index]
##
## Parameters:
##
##        index    : Specify a certain set to run (integer 1 - 238).
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        Run it again without an index to compile a plot.
##        The number of jobs should be a multiple of 17 * 7.
##
## Running Manually:
##        Rscript --vanilla experiments/regression/exp_scalability2.R 1:119
##        Rscript --vanilla experiments/regression/exp_scalability2.R
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressPackageStartupMessages({
    library(ggplot2)
    require(lemon)
    require(ggrepel)
})
source("experiments/regression/data.R")
source("experiments/regression/regression.R")
source("experiments/regression/utils.R")


## --------------------------------------------------
## Experiment Calculations
## --------------------------------------------------
calculate <- function(method,
                      data,
                      timelimit = 15000) {
    # Warmup (i.e. make sure any JIT has been executed)
    tmp <- robust_data(data_create(30, 5))
    method$train(tmp$X, tmp$Y, tmp$epsilon_sm, tmp$lambda_sm)

    # Time on real data
    time <- system.time(rr <- with_timelimit(
        method$train,
        timelimit,
        sprintf("%s %s", data$name, method$name),
        data$X,
        data$Y,
        data$epsilon_sm,
        data$lambda_sm
    ), gcFirst = TRUE)
    if (is.rr(rr)) {
        data.frame(
            method = method$name,
            dataset = data$name,
            n = nrow(data$X),
            d = ncol(data$X),
            time = time[[3]],
            eff = (time[[1]] + time[[2]]) / time[[3]],
            stringsAsFactors = TRUE
        )
    } else {
        data.frame()
    }
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_time <- function(df) {
    methods <- get_regression_methods()
    df <- df %>%
        group_by(dataset, method) %>%
        summarize(time = mean(time))
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    gg <- ggplot(df) +
        theme_bw() +
        theme_paper() +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        xlab(NULL) +
        ylab("Time [s]") +
        geom_text_repel(
            aes(1.05, time, label = method),
            force = 0.2,
            box.padding = 0.2,
            segment.alpha = 0.3,
            direction = "y",
            hjust = 0,
            nudge_x = 0.1
        ) +
        geom_point(aes(1, time, col = method, shape = method), size = 3) +
        theme(
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        ) +
        scale_x_continuous(limits = c(0.95, 2))
    reposition_legend(gg, "center", panel = "panel-4-2")
}

plot_time2 <- function(df) {
    methods <- get_regression_methods()
    df <- df %>%
        filter(dataset != "Synthetic") %>%
        group_by(dataset, method) %>%
        summarize(time = mean(time))
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
        ylab("Time [s]") +
        geom_text_repel(
            aes(1.05, time, label = method),
            force = 0.2,
            box.padding = 0.15,
            segment.alpha = 0.3,
            direction = "y",
            hjust = 0,
            nudge_x = 0.15
        ) +
        geom_point(aes(1, time, col = method, shape = method), size = 3) +
        theme(
            legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        ) +
        scale_x_continuous(limits = c(0.9, 2))
}

table_time <- function(df) {
    df <- df %>%
        filter(dataset != "Synthetic") %>%
        group_by(dataset, method) %>%
        summarize(time = mean(time))
    datasets <- levels(df$dataset)

    cat("\\begin{tabular}{l ", rep("r", length(methods)), "}\n", sep = "")
    cat("  \\hline \\toprule\n")
    cat("  \\textbf{Dataset} &\n  \\multicolumn{", length(methods), "}{c}{\\textbf{Time [s]}} \\\\\n", sep = "")
    cat(
        "  ",
        sprintf("& %s", get_nested(get_regression_methods(), "latex")),
        "\\\\\n",
        "  \\midrule\n"
    )
    for (d in datasets) {
        df2 <- df %>% filter(dataset == d)
        cat(" ", latex_name(d), sprintf("& %.1f", df2$time), "\\\\\n")
    }
    cat("  \\bottomrule\n\\end{tabular}\n")
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
            methods <- get_regression_methods()
            method_index <- (index - 1) %% length(methods) + 1
            data_index <- floor((index - 1) / length(methods)) + 1
            method <- methods[[method_index]]
            data <- data_at_index(data_index, seed_start = 742, normalise = TRUE)
            cat(sprintf("[scalability2] Init: %2d   %s   %s\n", index, method$name, data$name))

            df <- calculate(method, data)
            save_results(df, "scalability2", data_index, method$tag)
            cat(sprintf("[scalability2] Done: %2d   %s   %s\n", index, method$name, data$name))
        }
    } else {
        df <- load_results("scalability2")
        plot_pdf(plot_time2(df), "scalability2", 0.7, 0.5)
        # plot_pdf(plot_time2(df[df$n * df$d < 200 * 1000 | df$method != "SMDM" | df$time > 10, ]), "scalability2", 0.7, 0.5)
        # plot_pdf(plot_time(df), "scalability2", 0.8, 0.5)
    }
}