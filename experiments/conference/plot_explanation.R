## --------------------------------------------------
## Plot the results for the explanation experiments
##
##
## Usage:
##
##        Rscript --vanilla experiments/plot_explanation.R
##
## --------------------------------------------------

source("experiments/lime.R")
require(xtable)

DIRECTORY <- "experiments/results"

generate_explanations_emnist <- function(dir = DIRECTORY) {
    # Get data
    set.seed(42)
    emnist <- data_emnist(10000, classifier="digits2")
    selected <- 3
    expl <- slise.explain(emnist$X, emnist$Y, selected, epsilon = 0.1, lambda = 2, logit = TRUE)
    # Plot image
    cairo_pdf(file.path(dir, "explanation_emnist_digit.pdf"), 0.3*9, 0.3*9)
    plot(explain_img_slise_image(emnist$X[selected, ], colors=explain_slise_color_bw(), scale_colors=FALSE))
    dev.off()
    # Plot dist
    cairo_pdf(file.path(dir, "explanation_emnist_dist.pdf"), 0.3*9, 0.3*9)
    plot(ggplot() + theme_light() +
        geom_density(aes(emnist$Y, ..count.., linetype = "Dataset"), bw = 0.05) +
        geom_density(aes(emnist$Y[expl$subset], ..count.., linetype = "Subset"), bw = 0.05) +
        xlab("Class Probability") + ylab("Number of Samples") +
        theme(legend.position = "bottom", axis.text.y=element_blank(), legend.margin=ggplot2::margin(0, 0, 0, 0, unit='cm')) +
        guides(linetype=guide_legend(title=NULL))
    )
    dev.off()
    # Plot linear model
    cairo_pdf(file.path(dir, "explanation_emnist_model.pdf"), 0.4*8, 0.3*8)
    plot(explain_img_slise_image(expl$coefficients[-1], expl$x, 28, 28, class_labels=c("not 2", "is 2"), legend="right"))
    dev.off()
    # Plot lineup
    lineup <- explain_slise_get_lineup(expl, 6, FALSE, TRUE)
    images <- do.call(rbind, lapply(lineup$probabilities, function(p) expl$coefficients[-1]))
    cairo_pdf(file.path(dir, "explanation_emnist_lineup.pdf"), 8, 0.2*8)
    plot(explain_img_slise_lineup(images, paste("p =", lineup$probabilities), lineup$images, class_labels = c("not 2", "is 2"), legend = "right", nrow=1))
    dev.off()
    # Plot scatter
    set.seed(42)
    cairo_pdf(file.path(dir, "explanation_emnist_scatter.pdf"), 0.45*12, 0.35*12)
    plot(explain_img_slise_scatter(expl, lineup = lineup, scatter_size=0.035, logits=FALSE))
    dev.off()
    # Plot progression
    progression <- lapply(c(0.64, 0.32, 0.16, 0.08, 0.04, 0.02), function(e) {
        l <- (1 - 1 / exp(e * 8 - 0.1)) * 4.5
        slise.explain(emnist$X, emnist$Y, selected, epsilon = e, lambda = l, logit = TRUE)
    })
    imgs <- do.call(rbind, lapply(progression, function(p) p$coefficients[-1]))
    contours <- do.call(rbind, lapply(progression, function(p) p$x))
    labels <- lapply(progression, function(p) sprintf("ε = %.2f\nλ = %.2f, |S| = %.2f", p$epsilon, p$lambda, mean(p$subset)))
    cairo_pdf(file.path(dir, "explanation_emnist_epsilon.pdf"), 8, 0.25*8)
    plot(explain_img_slise_lineup(imgs, labels, contours, class_labels=c("not 2", "is 2"), legend="right", nrow=1))
    dev.off()
    # Plot limited
    emnist2 <- data_emnist(classifier="digits2")
    other <- 3
    mask <- emnist2$R == 2 | emnist2$R == other
    expl2 <- slise.explain(emnist2$X[mask,], emnist2$Y[mask], emnist$X[selected,], emnist$Y[selected],
                           epsilon = 0.1, lambda = 2, logit = TRUE)
    cairo_pdf(file.path(dir, "explanation_emnist_other.pdf"), 0.25*8, 0.3*8)
    plot(explain_img_slise_image(expl2$coefficients[-1], expl2$x, 28, 28, class_labels=c(other, "2"), legend="bottom"))
    dev.off()
}

generate_explanations_imdb <- function(dir = DIRECTORY) {
    print_latex <- function(text, tokens, expl) {
        alpha <- expl$alpha[-1]
        names(alpha) <- names(expl$scaled$scale_alpha(expl$coefficients))[-1]
        lim <- max(abs(alpha[expl$x != 0]))/2
        text_split <- stringr::str_split(text, " ")[[1]]
        for (i in seq_along(text_split)) {
            if (tokens[[i]] == "") next()
            v <- alpha[tokens[[i]]]
            if (is.na(v)) next()
            if (v > lim) {
                text_split[i] <- paste0("\\colorbox[HTML]{7FBC41}{", text_split[i], "}")
            } else if (v > lim / 3) {
                text_split[i] <- paste0("\\colorbox[HTML]{E6F5D0}{", text_split[i], "}")
            } else if (v < -lim) {
                text_split[i] <- paste0("\\colorbox[HTML]{DE77AE}{", text_split[i], "}")
            } else if (v < -lim / 3) {
                text_split[i] <- paste0("\\colorbox[HTML]{FDE0EF}{", text_split[i], "}")
            }
        }
        nt <- stringr::str_replace_all(do.call(paste, c(
            list("\\noindent\\fbox{\\parbox{0.98\\textwidth}{\\small\n\t"),
            text_split,
            list("\n}}\n\n")
        )), "&", "\\&")
        cat(nt)
    }
    imdb <- data_aclimdb(10000)

    # Find a review to compare to lime
    set.seed(42)
    imdb3 <- data_aclimdb(10000, model="lr")
    imdb4 <- data_aclimdb(model="lr")
    selected <- 10356
    review <- aclimdb_get_review(imdb4, selected)
    lime_expl <- lime_explain_aclimdb(imdb4, selected, 10)
    mask <- imdb3$X[, colnames(imdb3$X) == "street"] == 0
    slise_expl1 <- slise.explain(imdb3$X, imdb3$Y, imdb4$X[selected,], imdb4$Y[selected], epsilon=0.1, lambda=0.75, logit=TRUE)
    slise_expl2 <- slise.explain(imdb3$X[mask,], imdb3$Y[mask], imdb4$X[selected,], imdb4$Y[selected], epsilon=0.1, lambda=0.75, logit=TRUE)
    print_latex(review$text, review$tokens, slise_expl1)
    print_latex(review$text, review$tokens, slise_expl2)

    # Find a review with "wasn't bad"
    imdb2 <- data_aclimdb()
    mask <- which(imdb2$R == 1 & imdb2$Y < 0.5 & imdb2$X[, colnames(imdb2$X) == "wasnt"] > 0 & imdb2$X[, colnames(imdb2$X) == "bad"] > 0)
    for (selected in mask) {
        review <- aclimdb_get_review(imdb2, selected)
        expl <- slise.explain(imdb$X, imdb$Y, imdb2$X[selected,], imdb2$Y[selected], epsilon=0.1, lambda=0.5, logit=TRUE)
        if (expl$alpha[colnames(imdb2$X) == "wasnt"] == 0 || expl$alpha[colnames(imdb2$X) == "bad"] == 0)
            next()
        print(selected)
        print_latex(review$text, review$tokens, expl)
        break()
    }
}

generate_explanations_jets <- function(dir = DIRECTORY) {
    set.seed(42)
    jets <- data_jets(10000)
    selected <- 9
    expl <- slise.explain_find(jets$X, jets$Y, selected, epsilon=0.1, logit=TRUE, scale=TRUE, variables=2, lambda=100)
    print(expl)
    print(expl$alpha[-1])

    df <- data.frame(rbind(expl$x, expl$alpha[-1]))
    rownames(df) <- c("Jet", "α")
    colnames(df) <- names(expl$coefficients[-1])
    caption <- paste("Using \\slise to explain why this jet from the \\physics dataset is a ", c("Gluon.", "Quark.")[jets$R[selected]+1])
    table <- xtable(df, caption=caption, label="tab:exp:jets1")
    print(table, sanitize.colnames.function = identity, booktabs=TRUE, include.rownames=FALSE)
}

if (sys.nframe() == 0L) { # Only run with Rscript
    generate_explanations_emnist()
    generate_explanations_imdb()
    generate_explanations_jets()
}
