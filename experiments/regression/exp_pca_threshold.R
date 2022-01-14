## --------------------------------------------------
## Experiment for selecting the treshold for using PCA
##
##
## Usage:
##
##        Rscript --vanilla experiments/regression/exp_pca_threshold.R
##
## --------------------------------------------------

## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(source("experiments/regression/utils.R"))


## --------------------------------------------------
## Plotting
## --------------------------------------------------
plot_pca_treshold <- function(o = seq(0.3, 0.75, 0.01), d = 7:12, u = 500) {
    df <- expand.grid(o = o, d = d, u = u) %>%
        mutate(p = 1 - (1 - (1 - o)^d)^u, d = sprintf("%2d", d))
    df2 <- df %>% filter(o == 0.51) # %>% mutate(l = sprintf("%2d: %.2f", d, p))#   d = %d   p = %.2f", d, p))
    ggplot(df) +
        geom_vline(xintercept = 0.5, color = "grey", size = 1) +
        geom_line(aes(o, p, linetype = d, col = d), size = 1) +
        labs(
            x = "Fraction of outliers",
            y = "Probability of at least one\ncandidate with no outliers",
            linetype = "Dimensions",
            color = "Dimensions"
            # , title = "Probability of finding at least one subset without outliers") +
        ) +
        geom_text(data = df2, aes(o, p, label = d), hjust = 0.5, nudge_x = 0.02, vjust = 1) +
        scale_color_brewer(type = "qual", palette = "Dark2") +
        theme_bw() +
        theme_paper() +
        theme(legend.title = element_text())
}


## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    plot_pdf(plot_pca_treshold(), "pca_treshold", 0.5, 0.3)
}
