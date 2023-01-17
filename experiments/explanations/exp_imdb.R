## --------------------------------------------------
## Experiments that use SLISE for explaining IMDB
##
##
## Usage:
##
##  Rscript --vanilla experiments/explanations/exp_imdb.R
##
## --------------------------------------------------

source("experiments/explanations/utils.R")
source("experiments/explanations/data.R")
source("experiments/data/retrieve_aclimdb.R")
library(stringr)
library(tm)
library(SnowballC)

exp_imdb <- function() {
    set.seed(42)
    data <- data_imdb()
    strs <- aclimdb_get_str()
    subsample <- sample.int(nrow(data$X), 5000)

    # Find a review with "not bad" (wasn't, isn't, aren't, wouldn't, and couldn't)
    not_words <- c("wasnt", "isnt", "arent", "couldnt", "wouldnt")
    items <- which(data$R == 1 & data$Y < 0.5 & rowSums(data$X[, not_words]) > 0 & data$X[, "bad"] > 0)
    for (selected in items) {
        review <- get_imdb_review(selected, strs)
        if (length(review$split) > 250 || !str_detect(review$text, "((was)|(is)|(are)|(could)|(would))n't[^.!?:,;\\n]*bad")) {
            next()
        }
        cat(sprintf("Attempted review %5d", selected))
        mask <- data$X[selected, ] > 0
        expl <- slise.explain(
            data$X[subsample, mask],
            data$Y[subsample],
            data$epsilon,
            data$X[selected, mask],
            data$Y[selected],
            lambda1 = data$lambda1,
            lambda2 = data$lambda2,
            logit = TRUE
        )
        bad_weight <- expl$alpha["bad"]
        not_weight <- min(expl$alpha[not_words], na.rm = TRUE)
        cat(sprintf("   (%.3f, %.3f)\n", bad_weight, not_weight))
        if (bad_weight < -0.0001 || not_weight < 0.0001 || not_weight / bad_weight > 0.1) {
            cat("\nSelected review", selected, "\n")
            print_latex(review$split, review$tokens, expl)
        }
    }
}

get_imdb_review <- function(index, strs = NULL, set = "test", datadir = "experiments/data") {
    if (missing(strs)) {
        strs <- aclimdb_get_str(set, datadir)
    }
    text <- strs[[index]]
    text <- str_replace_all(text, "((<br />)|(\\n)|\n)", " \n ")
    text <- str_replace_all(text, "  ", " ")
    split <- str_split(text, " ")[[1]]
    corpus <- aclimdb_clean(VectorSource(split))
    tokens <- sapply(corpus$content, function(d) d$content)
    list(text = text, tokens = tokens, split = split)
}

print_latex <- function(text_split, tokens, expl) {
    alpha <- expl$alpha[-1]
    names(alpha) <- names(expl$alpha)[-1]
    lim <- (max(abs(alpha)) / 2) * 0.5 + median(abs(alpha)) * 0.5
    text_split <- str_replace_all(text_split, "&", "\\\\&")
    text_split <- str_replace_all(text_split, "\n", "\\\\\\\\\n")
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
    nt <- do.call(paste, c(
        list("\\noindent\\fbox{\\parbox{0.98\\textwidth}{\\small\n"),
        text_split,
        list("\n}}\n")
    ))
    cat(nt)
}

if (sys.nframe() == 0L) { # Only run with Rscript
    exp_imdb()
}