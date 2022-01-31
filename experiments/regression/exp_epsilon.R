## --------------------------------------------------
## Experiment showing the effect of epsilon
##
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_epsilon.R [index]
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 17.
##        Run it again without an index to compile a plot.
##
## Running locally:
##
##        Rscript --vanilla experiments/regression/exp_epsilon.R 1:17
##        Rscript --vanilla experiments/regression/exp_epsilon.R
##
##
## --------------------------------------------------

suppressMessages(require(ggplot2))
suppressMessages(require(lemon))
suppressMessages(source("experiments/regression/data.R"))
suppressMessages(source("experiments/regression/utils.R"))


fit_epsilon <- function(dataset, epsilon = c(seq(0.05, 0.5, 0.05), seq(0.6, 1.2, 0.1))) {
    do.call(rbind, lapply(epsilon, function(e) {
        slise <- slise.fit(
            dataset$X,
            dataset$Y,
            e,
            dataset$lambda * nrow(dataset$X) * e^2
        )
        data.frame(
            epsilon = e,
            dataset = dataset$name,
            error = c(dataset$Y - predict(slise)),
            stringsAsFactors = TRUE
        )
    }))
}


plot_size <- function(df) {
    df <- df %>%
        mutate(dataset = factor(dataset, unique(dataset))) %>%
        group_by(epsilon, dataset) %>%
        summarize(size = mean(abs(error) < epsilon[1]))
    ggplot(df) +
        geom_line(aes(epsilon, size, col = dataset, linetype = dataset), size = 1) +
        scale_linetype_discrete(name = NULL) +
        scale_color_brewer(type = "qual", palette = "Dark2", name = NULL) +
        ylim(c(0, 1)) +
        theme_bw() +
        theme_paper() +
        xlab("\u03B5") +
        ylab("Subset size")
}


plot_dist <- function(df, eps = c(0.2, 0.4, 0.6, 0.8, 1.0)) {
    df <- df %>%
        filter(epsilon %in% eps) %>%
        mutate(error = error / epsilon, epsilon = sprintf("%g", epsilon))
    gg <- ggplot(df) +
        geom_vline(xintercept = c(-1, 1), col = "grey") +
        geom_density(aes(error, col = epsilon, linetype = epsilon), size = 1) +
        scale_linetype_discrete(name = expression(epsilon)) +
        scale_color_brewer(type = "qual", palette = "Dark2", name = expression(epsilon)) +
        theme_bw() +
        theme_paper() +
        scale_x_continuous(breaks = c(-1, 0, 1), labels = c("-\u03B5", "0", "\u03B5")) +
        coord_cartesian(xlim = c(-1.2, 1.2)) +
        xlab("Relative Error") +
        ylab("Density") +
        theme(legend.title = element_text()) +
        facet_wrap(vars(dataset), ncol = 4, scale = "free")
    reposition_legend(gg, "center", panel = "panel-4-2")
}

plot_opt <- function(df) {
    opt <- function(ds, eps, error) {
        losses <- df %>%
            filter(dataset == ds) %>%
            # filter(epsilon == max(epsilon)) %>% # Most LASSO-like
            group_by(epsilon) %>%
            summarize(loss = loss_sharp_res(0, error^2, eps^2))
        loss_sharp_res(0, error^2, eps^2) / mean(losses$loss)
    }
    df <- df %>%
        group_by(epsilon, dataset) %>%
        summarize(opt = opt(dataset[[1]], epsilon[[1]], error))
    ggplot(df) +
        geom_line(aes(epsilon, opt, col = dataset, linetype = dataset)) +
        scale_linetype_discrete(name = NULL) +
        scale_color_brewer(type = "qual", palette = "Dark2", name = NULL) +
        theme_bw() +
        theme_paper() +
        xlab("\u03B5") +
        ylab("Loss / Mean Loss")
}

find_size <- function(df, target_size = 0.75) {
    df %>%
        group_by(dataset, epsilon) %>%
        summarize(size = mean(abs(error) < epsilon[1])) %>%
        group_by(dataset) %>%
        summarise(epsilon = inverse_interpolated(epsilon, size, target_size))
}

table_size <- function(df, sizes = c(0.75, 0.5, 0.3)) {
    epsilons <- lapply(sizes, find_size, df = df)
    cat("\\begin{tabular}{l ", rep("r", length(sizes)), "}\n", sep = "")
    cat("  \\hline \\toprule\n")
    cat("  \\textbf{Dataset} &\n\\multicolumn{", length(sizes), "}{c}{\\textbf{$\\varepsilon$ for subset sizes}}\\\\\n")
    cat("  ", sapply(sizes * 100, sprintf, fmt = "& \\unit{%.0f}{\\%%}"), "\\\\\n")
    cat("  \\midrule\n")
    for (d in unique(df$dataset)) {
        cat(
            "    ",
            latex_name(d),
            sapply(epsilons, function(e) sprintf("& $%.2f$", e$epsilon[e$dataset == d])),
            " \\\\\n"
        )
    }
    cat("  \\bottomrule\n\\end{tabular}\n")
}


## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        for (index in eval(parse(text = args[1]))) {
            data <- data_at_index(index, seed_start = 242, normalise = TRUE)
            cat(sprintf("[epsilon] Init: %2d   %s\n", index, data$name))

            res <- fit_epsilon(data)

            save_results(res, "epsilon", index)
            cat(sprintf("[epsilon] Done: %2d   %s\n", index, data$name))
        }
    } else {
        df <- load_results("epsilon")
        plot_pdf(plot_size(df), "epsilon_size", 0.5, 0.3)
        plot_pdf(plot_dist(df), "epsilon_dist", 1.0, 0.5)
        cat("\n\n")
        table_size(df)
    }
}