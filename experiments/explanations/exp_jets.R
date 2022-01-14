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


exp_jet_tab <- function(dir = "experiments/results") {
    set.seed(42)
    jets <- data_jets(n = 100000)
    x <- c(1196, 0.935, 0.002, 16) # old example
    selected <- which.min(rowSums(abs(scale(sweep(jets$X, 2, x), center = FALSE))))
    expl <- slise.explain(jets$X, jets$Y, 0.3, selected, logit = TRUE, normalise = TRUE, lambda1 = 10)
    print(expl)

    df <- data.frame(
        "Unnormalised Jet" = expl$x,
        "Unnormalised Model" = expl$coefficients[-1],
        "Unnormalised Impact" = expl$impact[-1],
        "_" = NA,
        "Normalised Jet" = expl$normalised_x,
        "Normalised Model" = expl$normalised[-1],
        "Normalised Impact" = expl$normalised_impact[-1]
    )
    rownames(df) <- names(expl$coefficients[-1])
    caption <- paste("Using \\slise to explain why this jet from the \\physics dataset is a", c("Gluon.", "Quark.")[jets$R[selected] + 1])
    table <- xtable(df, caption = caption, label = "tab:exp:jets1", digits = rep(3, 8), align = "lrrrcrrr")
    header1 <- "% \\textbf{Variables} & \\multicolumn{3}{c}{\\textbf{Unnormalised}} & & \\multicolumn{3}{c}{\\textbf{Normalised}} \\\\\n"
    header2 <- "& Jet & Model & Impact & & Jet & Model & Impact \\\\\n"
    print(table,
        booktabs = TRUE, comment = FALSE, include.colnames = FALSE, caption.placement = "top",
        add.to.row = list(pos = list(0, 0), command = c(header1, header2))
    )
}

exp_jet_img <- function(dir = "experiments/results") {
    set.seed(42)
    imgs <- data_jets(TRUE, 100000)
    selected <- 32
    expl <- slise.explain(imgs$X, imgs$Y, 0.5, selected, lambda1 = 200, logit = TRUE)
    print(expl)
    plot(expl, "pred")
    cairo_pdf(file.path(dir, "explanation_jet_img.pdf"), 0.7 * 9, 0.35 * 9)
    grid.draw(plot(expl, type = "image", plots = 2, labels = c("Gluon", "Quark"), title = "Physics Jet Image"))
    dev.off()
}

if (sys.nframe() == 0L) { # Only run with Rscript
    exp_jet_tab()
    exp_jet_img()
}