## --------------------------------------------------
## Peform experiment for selecting the best initialisation scheme
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/dami2021/exp_initialisation.R index
##
## Parameters:
##
##        index   : Specify the job index (1-170)
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 17.
##        Run it again without an index to compile a plot.
##
## Running Manually:
##        Rscript --vanilla experiments/dami2021/exp_initialisation.R 1:17
##        Rscript --vanilla experiments/dami2021/exp_initialisation.R
##
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressMessages(library(ggplot2))
suppressMessages(source("experiments/regression/data.R"))
suppressMessages(source("experiments/regression/utils.R"))

## --------------------------------------------------
## Calculation
## --------------------------------------------------
initialisation_calc <- function(dataset) {
    methods <- c(
        "lasso" = slise_initialisation_lasso,
        "ols" = slise_initialisation_ols,
        "zeros" = slise_initialisation_zeros,
        "cand 1" = slise_initialisation_candidates,
        "cand 2" = slise_initialisation_candidates2
    )
    do.call(rbind, lapply(seq_along(methods), function(b) {
        time <- system.time(
            slise <- slise.fit(
                dataset$X,
                dataset$Y,
                dataset$epsilon_sm,
                dataset$lambda_sm,
                initialisation = methods[[b]]
            )
        )[3]
        data.frame(
            dataset = dataset$name,
            initialisation = names(methods)[[b]],
            loss = slise$loss,
            subset = mean(slise$subset),
            time = time,
            stringsAsFactors = TRUE
        )
    }))
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_loss <- function(df) {
    ggplot(df) +
        theme_bw() +
        theme_paper() +
        geom_boxplot(aes(initialisation, loss)) +
        facet_wrap(vars(dataset), ncol = 4, scales = "free") +
        scale_y_continuous(labels = function(s) sprintf("%.2f", s)) +
        xlab("Initialisation Methods") +
        ylab("Loss")
}

plot_time <- function(df) {
    ggplot(df) +
        theme_bw() +
        theme_paper() +
        geom_boxplot(aes(initialisation, time)) +
        facet_wrap(vars(dataset), ncol = 4, scales = "free") +
        xlab("Initialisation Methods") +
        ylab("Time")
}

table_initialisations <- function(df, narrow = FALSE, qs = 0.1) {
    lower <- paste0("$\\qth{", round(qs * 100), "}$")
    upper <- paste0("$\\qth{", round((1 - qs) * 100), "}$")

    if (narrow) {
        cat("\\begin{tabular}{l c rrr c rrr}\n")
        cat("  \\hline \\toprule\n")
        cat("  \\textbf{Dataset} && \\multicolumn{3}{c}{\\textbf{Loss}} && \\multicolumn{3}{c}{\\textbf{Time [s]}} \\\\\n")
        cat("  Initialisation &&", lower, "& Median &", upper, "&&", lower, "& Median &", upper, "\\\\\n")
    } else {
        cat("\\begin{tabular}{ll c rrr c rrr}\n")
        cat("  \\hline \\toprule\n")
        cat("  \\textbf{Dataset} & \\textbf{Initialisation} && \\multicolumn{3}{c}{\\textbf{Loss}} && \\multicolumn{3}{c}{\\textbf{Time [s]}} \\\\\n")
        cat("  &&&", lower, "& Median &", upper, "&&", lower, "& Median &", upper, "\\\\\n")
    }
    start <- if (narrow) " " else " &"
    for (d in unique(df$dataset)) {
        df2 <- df %>% filter(dataset == d)
        cat("  \\midrule", if (narrow) paste0("\\textbf{", latex_name(d), "} \\\\") else latex_name(d), "\n")
        for (m in unique(df2$initialisation)) {
            df3 <- df2 %>% filter(initialisation == m)
            cat(start, sprintf(
                "%12s && %6.2f & %6.2f & %6.2f && %6.2f & %6.2f & %6.2f \\\\\n", paste0("\\", m),
                quantile(df3$loss, qs), median(df3$loss), quantile(df3$loss, 1 - qs),
                quantile(df3$time, qs), median(df3$time), quantile(df3$time, 1 - qs)
            ))
        }
    }
    cat("  \\bottomrule\n\\end{tabular}\n")
}

table_initialisations2 <- function(df, qs = 0.05) {
    lower <- paste0("$\\qth{", round(qs * 100), "}$")
    upper <- paste0("$\\qth{", round((1 - qs) * 100), "}$")

    cat("\\begin{tabular}{ll c rrr c r}\n")
    cat("  \\hline \\toprule\n")
    cat("  \\textbf{Dataset} & \\textbf{Initialisation} &&\n   \\multicolumn{3}{c}{\\textbf{Loss}} && \\textbf{Time [s]} \\\\\n")
    cat("  &&&", lower, "& Median &", upper, "&&", "Median \\\\\n")
    for (d in unique(df$dataset)) {
        df2 <- df %>% filter(dataset == d)
        cat("  \\midrule", latex_name(d), "\n")
        for (m in unique(df2$initialisation)) {
            df3 <- df2 %>% filter(initialisation == m)
            cat(" &", sprintf(
                "%12s && %6.2f & %6.2f & %6.2f && %6.2f  \\\\\n", paste0("\\", m),
                quantile(df3$loss, qs), median(df3$loss), quantile(df3$loss, 1 - qs),
                median(df3$time)
            ))
        }
    }
    cat("  \\bottomrule\n\\end{tabular}\n")
}

table_times <- function(df) {
    datasets <- levels(df$dataset)
    methods <- levels(df$initialisation)

    cat("\\begin{tabular}{l ", rep("r", length(methods)), "}\n", sep = "")
    cat("  \\hline \\toprule\n")
    cat("  \\textbf{Dataset} &\n  \\multicolumn{", length(methods), "}{c}{\\textbf{Median Time [s]}} \\\\\n", sep = "")
    cat(
        "  ",
        sprintf("& \\%s", methods),
        "\\\\\n",
        "  \\midrule\n"
    )
    for (d in datasets) {
        df2 <- df %>%
            filter(dataset == d) %>%
            group_by(initialisation) %>%
            summarise(time = median(time))
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
        for (index in eval(parse(text = args[1]))) {
            data <- data_at_index(index, seed_start = 342, normalise = TRUE)
            cat(sprintf("[initialisation] Init: %2d   %s\n", index, data$name))

            df <- initialisation_calc(data)

            save_results(df, "initialisation", index)
            cat(sprintf("[initialisation] Done: %2d   %s\n", index, data$name))
        }
    } else {
        df <- load_results("initialisation")
        df <- df[df$initialisation %in% c("lasso", "ols", "zeros", "cand 1"), ]
        df$initialisation <- factor(df$initialisation, labels = c("lasso", "ols", "zeros", "candidates"))
        plot_pdf(plot_loss(df), "initialisation_loss", 1.0, 0.4)
        cat("\n\n")
        table_times(df)
    }
}
