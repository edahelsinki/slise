## --------------------------------------------------
## Peform experiment for selecting the best number of candidates
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_candidates.R index
##
## Parameters:
##
##        index   : Specify the job index
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 17.
##        Run it again without an index to compile a plot.
##
## Running Manually:
##        Rscript --vanilla experiments/regression/exp_candidates.R 1:17
##        Rscript --vanilla experiments/regression/exp_candidates.R
##
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressMessages(require(ggplot2))
suppressMessages(require(ggrepel))
suppressMessages(require(tidyr))
suppressMessages(require(lemon))
suppressMessages(source("experiments/regression/data.R"))
suppressMessages(source("experiments/regression/utils.R"))


## --------------------------------------------------
## Calculation
## --------------------------------------------------
calc_num_init <- function(dataset, nums = c(50, 100, 200, 500, 1000)) {
    do.call(rbind, lapply(nums, function(n) {
        time <- system.time(
            slise <- slise.fit(
                dataset$X,
                dataset$Y,
                dataset$epsilon_sm,
                dataset$lambda_sm,
                num_init = n
            )
        )[3]
        data.frame(
            dataset = dataset$name,
            inits = n,
            time = time,
            loss = slise$value,
            subset = mean(slise$subset),
            stringsAsFactors = TRUE
        )
    }))
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_num_init <- function(df) {
    df <- df %>%
        gather(m, value, c("loss", "time")) %>%
        mutate(m = factor(m, labels = c("Loss", "Time"))) %>%
        group_by(dataset, m) %>%
        mutate(value = value / abs(median(value))) %>%
        group_by(dataset, m, inits) %>%
        summarise(value = median(value))
    ggplot(df, aes(inits, value, col = dataset, linetype = dataset)) +
        theme_bw() +
        theme_paper() +
        geom_line() +
        facet_wrap(vars(m), scales = "free") +
        scale_linetype_discrete(name = NULL) +
        scale_color_brewer(type = "qual", palette = "Dark2", name = NULL) +
        xlab("Number of Candidates") +
        ylab("Normalised Value")
}

table_candidates <- function(df) {
    sizes <- sort(unique(df$inits))
    datasets <- levels(df$dataset)
    cat("\\begin{tabular}{l ", rep("rr", length(datasets)), "}\n")
    cat("  \\hline \\toprule\n")
    cat("  \\textbf{Candidates}", sprintf("& \\multicolumn{2}{c}{%s}", sapply(datasets, latex_name)), "\\\\\n")
    cat("   ", rep("& Loss & Time", length(datasets)), "\\\\\n")
    cat("  \\midrule\n")
    for (s in sizes) {
        df2 <- df %>% filter(inits == s)
        cat(
            sprintf("  %2d", s),
            sapply(datasets, function(d) {
                df3 <- df2 %>% filter(dataset == d)
                sprintf("& $%.2f$ & $%.2f$", mean(df3$loss), mean(df3$time))
            }),
            " \\\\\n"
        )
    }
    cat("  \\bottomrule\n\\end{tabular}\n")
}

plot_scatter <- function(df) {
    df <- df %>%
        mutate(inits = as.factor(sprintf("%4d", inits))) %>%
        group_by(dataset, inits) %>%
        summarize(time = mean(time), loss = median(loss))
    gg <- ggplot(df) +
        theme_bw() +
        theme_paper() +
        geom_point(aes(time, loss, shape = inits, col = inits), size = 3) +
        geom_text_repel(
            aes(time, loss, label = inits),
            # hjust = "inward",
            # vjust = "inward",
            force = 0.4,
            min.segment.length = 0.75,
            box.padding = 0.15,
            nudge_x = 0.01
        ) +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_shape_manual(
            name = expression(u[init]),
            values = c(1, 2, 0, 3, 4)
        ) +
        scale_color_brewer(name = expression(u[init]), palette = "Dark2") +
        scale_y_continuous(labels = function(s) sprintf("%.2f", s)) +
        theme(legend.title = element_text()) +
        xlab("Time") +
        ylab("Loss")
    reposition_legend(gg, "center", panel = "panel-4-2")
}


## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        for (index in eval(parse(text = args[1]))) {
            data <- data_at_index(index, seed_start = 142, normalise = TRUE)
            cat(sprintf("[candidates] Init: %2d   %s\n", index, data$name))

            df <- calc_num_init(data)

            save_results(df, "candidates", index)
            cat(sprintf("[candidates] Done: %2d   %s\n", index, data$name))
        }
    } else {
        df <- load_results("candidates")
        # plot_pdf(plot_num_init(df), "candidates", 0.8, 0.4)
        plot_pdf(plot_scatter(df), "candidates2", 1.0, 0.4)
    }
}