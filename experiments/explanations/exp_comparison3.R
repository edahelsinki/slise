## --------------------------------------------------
## Compare the explanations from different model-agnostic post-hoc explainers
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/exp_comparison3.R [python]
##
## Parameters:
##
##        python  : Optional python executable
##
## --------------------------------------------------

suppressMessages(source("experiments/explanations/exp_comparison.R"))

plot_lineup <- function(digit = 2, selection = 1, dir = "experiments/results") {
    cat("Plotting a lineup of explanations\n")
    contour <- NULL
    heatmap <- NULL
    labels <- NULL
    set.seed(42)
    if (digit < 10) {
        # EMNIST
        offset <- digit * 8 + 1
        params <- get_config(offset)
        index <- which(params$Y > 0.5 & params$R == digit)[selection]
        params <- get_index(params, index)
        breaks <- NULL
    } else {
        # Jet Image
        params <- data_jets(TRUE, 5000, scale = TRUE, pred_fn = TRUE)
        params$index <- selection
        pred_fn <- params$pred_fn
        if (params$R[selection] == "Gluon") {
            params$Y <- 1 - params$Y
            params$pred_fn <- function(...) pred_fn(...)[, 1]
        }
        params$pred_fn <- function(...) pred_fn(...)[, 2]
        breaks <- c(0, 1 / attr(params$X, "scaled:scale"))
    }
    methods <- list(
        "slise" = slise_emnist,
        "shap (grey)" = function(...) shap_emnist(..., deletion = "grey", mid = 0.5),
        "shap (sample)" = function(...) shap_emnist(..., deletion = "sample"),
        "lime (original)" = function(...) lime_emnist(..., segmentation = "original"),
        "lime (small)" = function(...) lime_emnist(..., segmentation = "small"),
        "lime-slise" = limeslise_emnist
    )
    for (expl_fn in methods) {
        gc_rpy(TRUE)
        expl <- expl_fn(params$X, params$Y, params$index, params$pred_fn)
        contour <- rbind(contour, params$X[params$index, ])
        heatmap <- rbind(heatmap, if (is.null(expl$coefficients)) expl$impact else expl$coefficients)
        labels <- c(labels, expl$name)
        cat(" ", expl$name, "\n")
    }
    size <- sqrt(ncol(heatmap))
    name <- if (digit >= 10) "comparison_lineup_jets.pdf" else "comparison_lineup_emnist.pdf"
    cairo_pdf(file.path(dir, name), width = 0.8 * 9, height = 0.6 * 9)
    plot(plot_mnist(
        array(t(apply(heatmap, 1, function(x) x / quantile(abs(x), 0.99))), c(length(labels), size, size)),
        array(contour, c(length(labels), size, size)),
        c("not 2", "2"),
        enhance_colours = FALSE,
        breaks = breaks
    ) + facet_wrap(
        vars(Var1),
        nrow = 2,
        labeller = function(x) data.frame(Var1 = labels)
    ) + theme_image(legend.position = "None"))
    dev.off()
    cat("Lineup for", params$name, "saved to:", file.path(dir, name), "\n")
}

## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) > 0) {
        reticulate::use_python(args[1])
    }
    plot_lineup(2, 9)
    plot_lineup(10, 31)
}