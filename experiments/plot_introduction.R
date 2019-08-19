## --------------------------------------------------
## Plot the results for the introduction examples
##
##
## Usage:
##
##        Rscript --vanilla experiments/plot_introduction.R
##
## --------------------------------------------------

source("experiments/utils.R")
source("experiments/lime.R")

DIRECTORY <- "results"

generate_robust_regression_example <- function(dir = DIRECTORY) {
    set.seed(42)
    pox <- read.csv(file.path(DATA_DIR, "pox.csv"))
    x <- pox$fpox[!is.na(pox$fpox)]
    y <- pox$all[!is.na(pox$fpox)]
    y <- y[x < 6000]
    x <- x[x < 6000]
    slise <- slise.fit(x, y)
    ols <- lm(y~x)
    cairo_pdf(file.path(dir, "robust_regression_example.pdf"), 0.54 * 9, 2)
    plot(ggplot() + geom_point(aes(x, y), color="#909090") + theme_bw() +
        geom_abline(size=1, aes(
            linetype=c("SLISE", "OLS"),
            intercept=c(slise$coefficients[1], ols$coefficients[1]),
            slope=c(slise$coefficients[2], ols$coefficients[2]))) +
        scale_linetype_manual(name="", values=c("SLISE"=1, "OLS"=4)) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
        # xlab("Smallpox") + ylab("All Deaths")
    )
    dev.off()
}

generate_explanation_example <- function(low_edu = 0.1) {
    set.seed(42)
    data <- data_census()
    income <- data$income
    age <- data$age >= median(data$age)
    education <- data$education_num > 12 # Selecting College, Bachelors, Masters, and Doctorate
    # Balancing the data
    len <- sum(education) * low_edu
    mask <- sample(c(which(education), sample(which(!education), len)))
    income <- income[mask]
    age <- age[mask]
    education <- education[mask]
    data <- data[mask,]
    data2 <- data.frame(EDU=ifelse(!education, "low", "high"), AGE=ifelse(!age, "young", "old"), income=income)
    # Logistic Regression
    logreg <- glm(formula = income ~ EDU + AGE, family = "binomial", data = data2)
    print(glm(formula = income ~ age + education, family = "binomial")$coefficients)
    class(logreg) <- c("lr", class(logreg))
    model <- lime_model_wrapper(logreg, c("<=50K", ">50K"))
    # Select
    dfx <- data2[, -3]
    x <- cbind(as.numeric(education), as.numeric(age))
    colnames(x) <- c("EDU", "AGE")
    y <- predict(model, dfx)[, 2]
    explainer <- lime(dfx, model)
    # Explanations
    for (selected in c(which(age & education)[1], which(!age & education)[1], which(age & !education)[1], which(!age & !education)[1])) {
        cat("\nExplained:  ", paste0(colnames(x), " = ", x[selected,], ", "), paste0("Prediction = ", y[selected], "\n"))
        expl <- slise.explain_find(x, y, selected, logit=TRUE, epsilon=0.1, variables=1)
        cat("SLISE Explanation:  ", paste0(names(expl$coefficients[-1]), " = ", round(expl$coefficients[-1], 2), ",  "), "\n")
        expl <- lime::explain(dfx[selected,], explainer, n_labels=1, n_features = 1, dist_fun="euclidean")
        cat("LIME Explanation:   ", paste0("'", expl$feature_desc, "'", " = ", round(expl$feature_weight, 2), ",  "), "\n")
    }
}


if (sys.nframe() == 0L) { # Only run with Rscript
    generate_robust_regression_example()
    generate_explanation_example()
}