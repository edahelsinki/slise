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
##        index    : Specify a certain set to run (integer 1 - 70).
##
## Notes:
##        This is designed to be run in parallel on a cluster
##        since some of the configurations WILL pass the time
##        limit without honoring interrupts. If these are run
##        manually (without a cluster) then you might need to
##        cancel some executions (fastLTS et.c.) after a couple
##        of hours (all valid results are saved).
##        Run it again without an index to compile a plot.
##        The number of jobs should be a multiple of 7.
##
## Running Manually:
##        Rscript --vanilla experiments/regression/exp_scalability.R 1:7
##        Rscript --vanilla experiments/regression/exp_scalability.R
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))
source("experiments/regression/data.R")
source("experiments/regression/regression.R")
source("experiments/regression/utils.R")


## --------------------------------------------------
## Experiment Calculations
## --------------------------------------------------
calculate <- function(method,
                      data_index,
                      timelimit = 1000,
                      ns = c(110, 500, 1000, 5000, 10000, 50000, 100000),
                      ds = c(10, 50, 100, 225, 500, 1000, 2250, 5000)) {
    # Warmup (i.e. make sure any JIT has been executed)
    tmp <- robust_data(data_create(30, 5))
    method$train(tmp$X, tmp$Y, tmp$epsilon_sm, tmp$lambda_sm)

    # Setup configs
    config <- expand.grid(n = ns, d = ds) %>%
        filter(n >= d & (d == 100 | n == 10000)) %>%
        mutate(ord = d * n + d) %>%
        arrange(ord)
    df <- data.frame()
    lns <- c()
    lds <- c()

    # Run the configs
    for (i in 1:nrow(config)) {
        n <- config$n[[i]]
        d <- config$d[[i]]
        # Check if the limit has been reached previously
        if (any(n >= lns & d >= lds)) {
            next
        }
        set.seed(n * 1000 + d + data_index + 42)
        data <- robust_data(data_create(n, d))
        time <- system.time(rr <- with_timelimit(
            method$train,
            timelimit * 5,
            sprintf("%d x %d %s", n, d, method$name),
            data$X,
            data$Y,
            data$epsilon_sm,
            data$lambda_sm
        ), gcFirst = TRUE)
        if (is.rr(rr)) {
            df <- rbind(df, data.frame(
                method = method$name,
                n = n,
                d = d,
                time = time[[3]],
                eff = (time[[1]] + time[[2]]) / time[[3]],
                stringsAsFactors = TRUE
            ))
            save_results(df, "scalability", data_index, method$tag)
        }
        if (time[[3]] > timelimit) {
            lns <- c(lns, n)
            lds <- c(lds, d)
        }
    }
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_time <- function(df) {
    methods <- get_regression_methods()
    df <- df %>%
        group_by(n, d, method) %>%
        summarize(time = mean(time)) %>%
        mutate(method = factor(method, get_nested(methods, "name")))
    labels <- c("Number of Items", "Number of Dimensions")
    labels <- factor(labels, labels, ordered = TRUE)
    df <- rbind(
        df %>% filter(d == 100) %>% mutate(variant = labels[1], param = n),
        df %>% filter(n == 10000, d < 5000, d > 10) %>% mutate(variant = labels[2], param = d)
    ) # %>% filter(time < 1500 | method == "Sparse LTS")
    ggplot(df) +
        theme_bw() +
        theme_paper() +
        geom_line(aes(param, time, col = method, linetype = method, size = method)) +
        geom_point(aes(param, time, col = method, shape = method), size = 3) +
        facet_wrap(vars(variant), scales = "free") +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_size_manual(values = get_nested(methods, "size"), name = NULL) +
        xlab(NULL) +
        ylab("Time [s]") +
        scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
        # breaks = c(10, 50, 100, 500, 1000, 10000, 100000),
        # labels = c("10", "50", "100", "500 ", " 1000", "10 000", "100 000")) +
        scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), labels = paste) +
        # geom_rect(aes(xmin = 1080, xmax = 2800, ymin = 1080, ymax = 2000), fill = "white") +
        # geom_rect(aes(xmin = 320, xmax = 900, ymin = 1100, ymax = 2000), fill = "white") +
        geom_hline(yintercept = 1000, linetype = 2, size = 1, col = "black") +
        coord_trans(ylim = c(0.02, 800)) +
        theme(legend.key.width = grid::unit(2, "lines"))
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

            methods <- get_regression_methods()
            method <- methods[[(index - 1) %% length(methods) + 1]]
            data_index <- as.integer((index - 1) / length(methods))
            cat(sprintf("[scalability] Init: %2d   %s\n", index, method$name))

            calculate(method, data_index)
            cat(sprintf("[scalability] Done: %2d   %s\n", index, method$name))
        }
    } else {
        df <- load_results("scalability")
        plot_pdf(plot_time(df), "scalability", 1.0, 0.4)
    }
}