## --------------------------------------------------
## Peform experiment for selecting the best beta_max
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_beta_max.R index
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
##        Rscript --vanilla experiments/regression/exp_beta_max.R 1:17
##        Rscript --vanilla experiments/regression/exp_beta_max.R
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
calc_beta_max <- function(dataset, maxes = c(0.5, 1, 5, 10, 20, 30)) {
    do.call(rbind, lapply(maxes, function(b) {
        time <- system.time(
            slise <- slise.fit(
                dataset$X,
                dataset$Y,
                dataset$epsilon_sm,
                dataset$lambda_sm,
                beta_max = b / dataset$epsilon_sm^2
            )
        )[3]
        data.frame(
            dataset = dataset$name,
            beta = b,
            time = time,
            loss = slise$value,
            subset = mean(slise$subset),
            stringsAsFactors = TRUE
        )
    }))
}


## --------------------------------------------------
## Plot
## --------------------------------------------------
plot_beta_max <- function(df) {
    df <- df %>%
        gather(m, value, c("loss", "time")) %>%
        mutate(m = factor(m, labels = c("Loss", "Time [s]"))) %>%
        group_by(dataset, m) %>%
        mutate(value = value / median(abs(value))) %>% # / abs(median(value))) %>%
        group_by(dataset, m, beta) %>%
        mutate(value = median(value))
    ggplot(df, aes(beta, value, col = dataset, linetype = dataset)) +
        theme_bw() +
        theme_paper() +
        geom_line() +
        facet_wrap(vars(m), nrow = 1, scales = "free_y") +
        scale_linetype_discrete(name = NULL) +
        scale_color_brewer(type = "qual", palette = "Dark2", name = NULL) +
        xlab(expression(beta[max] ~ epsilon^2)) +
        ylab("Normalised Value")
}

plot_dataset <- function(df) {
    df <- df %>%
        group_by(dataset, beta) %>%
        summarize(Time = mean(time), Loss = mean(loss)) %>%
        gather(m, value, c("Loss", "Time"))
    ggplot(df, aes(beta, value)) +
        theme_bw() +
        theme_paper() +
        geom_line() +
        facet_wrap(vars(dataset, m), nrow = 4, scales = "free") +
        scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
        theme(legend.title = element_text()) +
        theme(strip.text = element_text(margin = margin(0, 0, 0.1, 0, "cm"))) +
        xlab(expression(beta[max] ~ epsilon^2)) +
        ylab(NULL)
}

plot_scatter <- function(df) {
    df <- df %>%
        mutate(beta = factor(sprintf("%2g", beta), sprintf("%2g", unique(beta)))) %>%
        group_by(dataset, beta) %>%
        summarize(time = mean(time), loss = mean(loss))
    labels <- sapply(levels(df$beta), function(l) as.expression(bquote(.(l)/epsilon^2)))
    gg <- ggplot(df) +
        theme_bw() +
        theme_paper() +
        geom_point(aes(time, loss, shape = beta, col = beta), size = 3) +
        geom_text_repel(
            aes(time, loss, label = beta),
            # hjust = "inward",
            # vjust = "inward",
            force = 0.35,
            min.segment.length = 1,
            segment.alpha = 0.3,
            box.padding = 0.1,
            nudge_x = 0.01
        ) +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_shape_manual(
            name = expression(beta[max]),
            values = c(1, 2, 0, 3, 4, 18, 20),
            labels = labels
        ) +
        scale_color_brewer(
            name = expression(beta[max]),
            palette = "Dark2",
            labels = labels
        ) +
        theme(legend.title = element_text()) +
        xlab("Time") +
        ylab("Loss") +
        guides(col = guide_legend(ncol = 2), shape = guide_legend(ncol = 2))
    reposition_legend(gg, "center", panel = "panel-4-2")
}


## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        for (index in eval(parse(text = args[1]))) {
            set.seed(42 + index)

            data <- data_at_index(index, seed_start = 42, normalise = TRUE)
            cat(sprintf("[beta_max] Init: %2d   %s\n", index, data$name))

            df <- calc_beta_max(data)

            save_results(df, "beta_max", index)
            cat(sprintf("[beta_max] Done: %2d   %s\n", index, data$name))
        }
    } else {
        df <- load_results("beta_max")
        # plot_pdf(plot_beta_max(df), "beta_max", 0.8, 0.3)
        # plot_pdf(plot_dataset(df), "beta_max2", 1.0, 0.8)
        plot_pdf(plot_scatter(df), "beta_max_scatter", 1.0, 0.4)
    }
}