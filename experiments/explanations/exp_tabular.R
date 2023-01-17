## --------------------------------------------------
## Experiments that use SLISE for tabular explanations
##
##
## Usage:
##
##  Rscript --vanilla experiments/explanations/exp_tabular.R
##
## --------------------------------------------------

source("experiments/explanations/utils.R")
source("experiments/explanations/data.R")

exp_mtcars <- function() {
    # This dataset so small that the explanations are relatively unstable
    cat("mtcars\n")
    set.seed(42)
    # data <- data_mtcars("lm")
    # cat("Linear Model MSE: ", mean((data$Y - data$R)^2), "\n")
    data <- data_mtcars("rf")
    cat("Random Forest MSE:", mean((data$Y - data$R)^2), "\n")
    expl <- slise.explain(data$X, data$Y, data$epsilon, 1, lambda1 = data$lambda1, normalise = TRUE)
    print(expl, 20)
    cat("Random Forest Importance\n")
    imp <- randomForest::importance(data$model_obj)
    print(imp[order(-abs(expl$normalised[-1])), ])
    # plot(ne, "dist")
}

exp_tabbincl <- function() {
    # TODO: Find some nice tabular dataset with a binary classification
}

if (sys.nframe() == 0L) { # Only run with Rscript
    exp_mtcars()
    exp_boston()
}