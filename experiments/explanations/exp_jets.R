## --------------------------------------------------
## Experiments that use SLISE for explaining jets
##
##
## Usage:
##
##        Rscript --vanilla experiments/explanations/exp_jets.R
##
## --------------------------------------------------

source("experiments/explanations/utils.R")
source("experiments/explanations/data.R")
library(xtable)
library(stringr)
library(grid)


exp_jet_tab <- function() {
    set.seed(42)
    all_jets <- data_jets(scale = FALSE)
    selected <- 235830 # c(1196, 0.0195, 33, 20, 0.934, 0.00172, 16)
    jets <- data_jets(n = 10000, scale = FALSE)
    expl <- slise.explain(
        jets$X,
        jets$Y,
        jets$epsilon,
        all_jets$X[selected, ],
        all_jets$Y[selected],
        logit = TRUE,
        normalise = TRUE,
        lambda1 = jets$lambda1,
        lambda2 = jets$lambda2
    )
    print(expl)

    df <- data.frame(
        "Unnormalised Jet" = expl$x,
        "_" = NA,
        "Normalised Jet" = expl$normalised_x,
        "Normalised Model" = expl$normalised[-1],
        "Normalised Term" = expl$normalised_term[-1],
        check.names = FALSE
    )
    rownames(df) <- names(expl$coefficients[-1])
    df <- df[order(abs(df[["Normalised Term"]]), decreasing = TRUE), ]
    table <- xtable(df, caption = "TODO", label = "tab:exp:jet_tab", digits = 3, align = "lrcrrr")
    header1 <- "% \\textbf{Variables} & \\multicolumn{1}{c}{\\textbf{Unnormalised}} & & \\multicolumn{3}{c}{\\textbf{Normalised}} \\\\\n"
    header2 <- "& Jet & & Normalised & Model & Term \\\\\n"
    print(table,
        booktabs = TRUE, comment = FALSE, include.colnames = FALSE, caption.placement = "top",
        add.to.row = list(pos = list(0, 0), command = c(header1, header2))
    )
}

exp_jet_img <- function(dir = "experiments/results") {
    set.seed(42)
    imgs <- data_jets(TRUE, 5000, scale = TRUE)
    selected <- 31
    labels <- c("Gluon", "Quark")
    if (imgs$R[selected] == "Gluon") {
        imgs$Y <- 1 - imgs$Y
        labels <- labels[2:1]
    }
    expl <- slise.explain(
        imgs$X,
        imgs$Y,
        imgs$epsilon,
        selected,
        lambda1 = imgs$lambda1,
        lambda2 = imgs$lambda2,
        logit = TRUE
    )
    # print(expl)
    # plot(expl, "pred")
    expl$x <- expl$x * attr(imgs$X, "scaled:scale")
    cairo_pdf(file.path(dir, "explanation_jet_img.pdf"), 0.7 * 9, 0.35 * 9)
    grid.draw(plot(expl, type = "image", plots = 2, labels = labels, title = "Jet Image: %s", breaks = 0:1))
    dev.off()
    cat("Image saved to:", file.path(dir, "explanation_jet_img.pdf"), "\n")
}

if (sys.nframe() == 0L) { # Only run with Rscript
    exp_jet_tab()
    exp_jet_img()
}