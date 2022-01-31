## --------------------------------------------------
## Peform experiment for selecting the best number of iterations
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_iterations.R index
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
##        Rscript --vanilla experiments/regression/exp_iterations.R 1:17
##        Rscript --vanilla experiments/regression/exp_iterations.R
##
## --------------------------------------------------


## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressMessages(require(ggplot2))
suppressMessages(require(tidyr))
suppressMessages(require(lemon))
suppressMessages(source("experiments/regression/data.R"))
suppressMessages(source("experiments/regression/utils.R"))


## --------------------------------------------------
## Calculation
## --------------------------------------------------
calc_iterations <- function(data,
                            iters = c(100, 200, 250, 300, 400),
                            approxes = c(1.05, 1.075, 1.10, 1.15, 1.20)) {
    grid <- expand.grid(iters = iters, approx = approxes)
    do.call(rbind, mapply(function(b, a) {
        time <- system.time(
            slise <- slise.fit(
                data$X,
                data$Y,
                data$epsilon_sm,
                data$lambda_sm,
                max_iterations = b,
                max_approx = a
            )
        )[3]
        data.frame(
            dataset = data$name,
            iterations = b,
            approx = a,
            time = time,
            loss = slise$value,
            subset = mean(slise$subset),
            stringsAsFactors = TRUE
        )
    }, grid$iters, grid$approx, SIMPLIFY = FALSE))
}


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_iterations <- function(df) {
    df <- df %>%
        mutate(approx = as.factor(sprintf("%.2f", approx))) %>%
        gather(m, value, c("loss", "time")) %>%
        mutate(m = factor(m, labels = c("Loss", "Time"))) %>%
        group_by(dataset, m) %>%
        mutate(value = value / abs(median(value))) %>%
        group_by(dataset, m, iterations, approx) %>%
        summarize(value = median(value))
    levels(df$approx) <- sapply(levels(df$approx), function(v) as.expression(bquote(r[max] == .(v))))
    ggplot(df, aes(iterations, value, col = dataset, linetype = dataset)) +
        theme_bw() +
        theme_paper() +
        geom_line() +
        facet_grid(rows = vars(m), cols = vars(approx), scales = "free", labeller = label_parsed) +
        scale_linetype_discrete(name = NULL) +
        scale_color_brewer(type = "qual", palette = "Dark2", name = NULL) +
        xlab(expression(Iterations ~ ~ (i[max]))) +
        ylab("Normalised Value") +
        geom_hline(aes(yintercept = -Inf), size = 0.75) +
        coord_cartesian(clip = "off")
}

plot_dataset <- function(df) {
    df <- df %>%
        mutate(approx = as.factor(sprintf("%.2f", approx))) %>%
        group_by(dataset, iterations, approx) %>%
        summarize(Time = mean(time), Loss = mean(loss)) %>%
        gather(m, value, c("Loss", "Time"))
    gg <- ggplot(df, aes(iterations, value, col = approx, linetype = approx, shape = approx)) +
        theme_bw() +
        theme_paper() +
        geom_line(size = 1) +
        geom_point(size = 3) +
        facet_wrap(vars(dataset, m), scales = "free", dir = "v") +
        scale_linetype_discrete(name = "K") +
        scale_color_brewer(name = "K", type = "qual", palette = "Dark2") +
        scale_shape_manual(name = "K", values = c(1, 2, 0, 3, 4)) +
        # scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
        xlab(expression(Iterations ~ ~ (i[max]))) +
        ylab(NULL) +
        theme(
            strip.text = element_text(margin = margin(0, 0, 0.1, 0, "cm")),
            legend.title = element_text(),
            legend.key.width = grid::unit(2, "lines")
        )
    reposition_legend(gg, "center", panel = c("panel-3-4", "panel-4-4"))
}

plot_dataset2 <- function(df) {
    df <- df %>%
        mutate(iterations = as.factor(paste(iterations))) %>%
        group_by(dataset, iterations, approx) %>%
        summarize(Time = mean(time), Loss = mean(loss)) %>%
        gather(m, value, c("Loss", "Time"))
    ggplot(df, aes(approx, value, col = iterations, linetype = iterations, shape = iterations)) +
        theme_bw() +
        theme_paper() +
        geom_line() +
        geom_point() +
        facet_wrap(vars(dataset, m), scales = "free") +
        scale_linetype_discrete(name = expression(i[max])) +
        scale_color_brewer(name = expression(i[max]), type = "qual", palette = "Dark2") +
        scale_shape_manual(name = expression(i[max]), values = c(1, 2, 0, 3, 4)) +
        scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
        xlab("Approximation Ratio, K") +
        ylab(NULL) +
        theme(
            strip.text = element_text(margin = margin(0, 0, 0.1, 0, "cm")),
            legend.title = element_text()
        )
}

plot_scatter <- function(df) {
    df <- df %>%
        mutate(
            approx = as.factor(sprintf("%.2f", approx)),
            iterations = as.factor(paste(iterations))
        ) %>%
        group_by(dataset, iterations, approx) %>%
        summarize(Time = mean(time), Loss = mean(loss))
    ggplot(df, aes(Time, Loss, shape = iterations, col = approx)) +
        theme_bw() +
        theme_paper() +
        geom_point() +
        facet_wrap(vars(dataset), nrow = 2, scales = "free") +
        scale_shape_manual(
            name = expression(i[max]),
            values = c(1, 2, 0, 3, 4)
        ) +
        scale_color_brewer(name = "K", palette = "Dark2") +
        scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
        theme(legend.title = element_text())
}


## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        for (index in eval(parse(text = args[1]))) {
            data <- data_at_index(index, seed_start = 442, normalise = TRUE)
            cat(sprintf("[iterations] Init: %2d   %s\n", index, data$name))

            df <- calc_iterations(data)

            save_results(df, "iterations", index)
            cat(sprintf("[iterations] Done: %2d   %s\n", index, data$name))
        }
    } else {
        df <- load_results("iterations")
        # plot_pdf(plot_iterations(df), "iterations", 1.0, 0.5)
        plot_pdf(plot_dataset(df), "iterations2", 1.0, 0.8)
        # plot_pdf(plot_scatter(df), "iterations3", 1.0, 0.4)
    }
}