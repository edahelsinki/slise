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
library(stringr)
library(tm)

exp_imdb <- function(dir = "experiments/results") {
    set.seed(42)
    data <- data_imdb()

    # Find a review with "not bad" (wasn't, isn't, aren't, wouldn't, and couldn't)
    not_words <- c("wasnt", "isnt", "arent", "couldnt", "wouldnt")
    items <- which(data$R == 1 & data$Y < 0.5 & rowSums(data$X[, not_words]) > 0 & data$X[, "bad"] > 0)
    for (selected in items) {
        review <- get_imdb_review(selected)
        if (length(review$split) > 250 || !str_detect(review$text, "((was)|(is)|(are)|(could)|(would))n't[^.!?:,;\\n]*bad")) {
            next()
        }
        mask <- data$X[selected, ] != 0
        expl <- slise.explain(data$X[, mask], data$Y, 1.0, selected, lambda1 = 10, logit = TRUE)
        if (expl$alpha["bad"] >= 0 || min(expl$alpha[not_words], na.rm = TRUE) / expl$alpha["bad"] < 1 / 8) {
            next()
        }
        print(selected)
        print_latex(review$split, review$tokens, expl)
    }

    # Find a review to compare to lime
    # set.seed(42)
    # imdb3 <- data_aclimdb(10000, model="lr")
    # imdb4 <- data_aclimdb(model="lr")
    # selected <- 10356
    # review <- aclimdb_get_review(imdb4, selected)
    # lime_expl <- lime_explain_aclimdb(imdb4, selected, 10)
    # mask <- imdb3$X[, colnames(imdb3$X) == "street"] == 0
    # slise_expl1 <- slise.explain(imdb3$X, imdb3$Y, 0.1, imdb4$X[selected, ], imdb4$Y[selected], lambda1=0.75, logit=TRUE)
    # slise_expl2 <- slise.explain(imdb3$X[mask, ], 0.1, imdb3$Y[mask], imdb4$X[selected, ], imdb4$Y[selected], lambda1=0.75, logit=TRUE)
    # print_latex(review$text, review$tokens, slise_expl1)
    # print_latex(review$text, review$tokens, slise_expl2)
}

get_imdb_review <- function(index, set = "test", datadir = "experiments/data") {
    strs <- readRDS(file.path(datadir, paste0("aclimdb_str_", set, ".rds")))
    text <- if (index <= length(strs$pos)) strs$pos[[index]] else strs$neg[[index - length(strs$pos)]]
    text <- str_replace_all(text, "((<br />)|(\\n)|\n)", " \n ")
    text <- str_replace_all(text, "  ", " ")
    split <- str_split(text, " ")[[1]]
    corpus <- aclimdb_clean(VectorSource(split))
    tokens <- sapply(corpus$content, function(d) d$content)
    list(text = text, tokens = tokens, split = split)
}

aclimdb_clean <- function(source) {
    data <- VCorpus(source, readerControl = list(language = "en"))
    data <- tm_map(data, content_transformer(function(x) gsub("((<br />)|(\\\\n)|(\\n))", " ", x)))
    data <- tm_map(data, content_transformer(function(x) gsub("(<|>|\\(|\\)|\\[|\\]|\\{|\\})", " ", x)))
    data <- tm_map(data, stripWhitespace)
    data <- tm_map(data, content_transformer(tolower))
    data <- tm_map(data, content_transformer(removePunctuation))
    data <- tm_map(data, content_transformer(removeNumbers))
    data <- tm_map(data, removeWords, stopwords("english"))
    data <- tm_map(data, stemDocument)
    data
}

print_latex <- function(text_split, tokens, expl) {
    alpha <- expl$alpha[-1]
    names(alpha) <- names(expl$alpha)[-1]
    lim <- max(abs(alpha)) / 2
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