## --------------------------------------------------
## This experiment is showing that the SLISE algorithm is optimal for solving the problem.
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_optimality.R[index]
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 17.
##        Run it again without an index to compile a plot.
##
## Running locally:
##
##        Rscript --vanilla experiments/regression/exp_optimality.R 1:17
##        Rscript --vanilla experiments/regression/exp_optimality.R
##
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressPackageStartupMessages({
    require(ggplot2)
    require(lemon)
})
source("experiments/regression/data.R")
source("experiments/regression/regression.R")
source("experiments/regression/utils.R")


## --------------------------------------------------
## Calculation
## --------------------------------------------------
optimality_calculate <- function(data,
                                 points = seq(0.5, 2.5, 0.25),
                                 timelimit = 6000) {
    methods <- get_regression_methods()
    names <- get_nested(methods, "name")
    models <- lapply(methods, function(m) {
        with_timelimit(
            m$train,
            timelimit,
            sprintf("[optimality] %s %s", data$name, m$name),
            data$X,
            data$Y,
            data$epsilon_sm,
            data$lambda_sm
        )
    })
    epsilons <- points * data$epsilon_sm
    do.call(rbind, lapply(which(sapply(models, is.rr)), function(i) {
        mod <- models[[i]]
        res2 <- (predict(mod, data$X) - data$Y)^2
        losses <- sapply(epsilons, function(eps) {
            loss_sharp_res(mod$coef, res2, eps^2)
        })
        data.frame(
            dataset = data$name,
            loss = losses,
            epsilon = epsilons,
            ref_eps = data$epsilon_sm,
            method = names[[i]],
            stringsAsFactors = TRUE
        )
    }))
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_optimality <- function(df) {
    methods <- get_regression_methods()
    df <- df %>%
        group_by(method, dataset, epsilon, ref_eps) %>%
        summarise(loss = mean(loss)) %>%
        group_by(dataset, epsilon) %>%
        mutate(loss = loss / median(abs(loss)))
    df$dataset <- factor(df$dataset, data_name_factor())
    df$method <- factor(df$method, get_nested(methods, "name"))
    gg <- ggplot(df) +
        theme_bw() +
        theme_paper() +
        geom_vline(aes(xintercept = ref_eps), col = "black") +
        geom_line(aes(epsilon, loss, col = method, linetype = method, size = method)) +
        geom_point(aes(epsilon, loss, col = method, shape = method), size = 3) +
        scale_linetype_manual(values = get_nested(methods, "linetype"), name = NULL) +
        scale_color_manual(values = get_nested(methods, "color"), name = NULL) +
        scale_shape_manual(values = get_nested(methods, "shape"), name = NULL) +
        scale_size_manual(values = get_nested(methods, "size"), name = NULL) +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        xlab(expression("Loss function" ~ epsilon)) +
        ylab("Relative Loss") +
        geom_label(aes(ref_eps, Inf, label = "epsilon"), parse = TRUE, vjust = 0.8, hjust = 0.5) +
        coord_cartesian(clip = "off") +
        theme(legend.key.width = grid::unit(2, "lines"))
    reposition_legend(gg, "center", panel = "panel-4-2")
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
            data <- data_at_index(index, seed_start = 542, normalise = TRUE)
            cat("[optimality] Init:", index, "\n")

            df <- optimality_calculate(data)

            save_results(df, "optimality", index)
            cat("[optimality] Done:", index, "\n")
        }
    } else {
        df <- load_results("optimality")
        plot_pdf(plot_optimality(df), "optimality", 1.0, 0.6)
    }
}