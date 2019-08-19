## --------------------------------------------------
## Retrieve and preprocess the
##    "Large Movie Review Dataset"
##
##    http://ai.stanford.edu/~amaas/data/sentiment/
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla retrieve_aclimdb.R destdir
##
## This will download the data, preprocess it
## and create an rds-file containing training
## and testing data.
##
## The argument 'destdir' specifies the destination directory
## --------------------------------------------------

## --------------------------------------------------
## Libraries
## --------------------------------------------------
library(tm)
library(qdap)
library(Matrix)
library(SnowballC)

library(e1071)
library(randomForest)
library(elmNNRcpp)

source("utils.R")

aclimdb_clean <- function(source) {
    data <- VCorpus(source, readerControl = list(language = "en"))
    data <- tm_map(data, content_transformer(function(x) gsub("<br />", " ", x)))
    data <- tm_map(data, stripWhitespace)
    data <- tm_map(data, content_transformer(tolower))
    data <- tm_map(data, content_transformer(removePunctuation))
    data <- tm_map(data, content_transformer(removeNumbers))
    data <- tm_map(data, content_transformer(qdap::bracketX))
    data <- tm_map(data, removeWords, stopwords("english"))
    data <- tm_map(data, stemDocument)
    data
}

aclimdb_get_review <- function(data, index, set="test") {
    id <- rownames(data$X)[[index]]
    cls <- if (data$R[[index]] == 0) "neg" else "pos"
    path <- file.path("experiments/data/aclImdb", set, cls, id)
    file <- file(path, encoding = "UTF-8")
    review <- paste(readLines(file, warn = FALSE))
    close(file)
    review <- stringr::str_replace_all(review, "<br /><br />", " ")
    aclimbd_vectorise(review, colnames(data$X))
}

aclimbd_vectorise <- function(review, cols) {
    data <- aclimdb_clean(VectorSource(stringr::str_split(review, " ")[[1]]))
    vec <- c(rep(0, length(cols)))
    tokens <- sapply(data$content, function(d) d$content)
    for (d in tokens) {
        sel <- which(d == cols)
        vec[sel] <- vec[sel] + 1
    }
    vec <- vec / max(vec)
    list(vec = vec, tokens = tokens, text = review)
}


# This is only run when called from Rscript
if (sys.nframe() == 0L) {

    ## --------------------------------------------------
    ## Command-line arguments
    ## --------------------------------------------------
    args    <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 1)
        destdir <- "experiments/data"
    else
        destdir <- args[1]

    ## --------------------------------------------------
    ## Retrieve the data
    ## --------------------------------------------------
    cat("Downloading Data\n")
    dataURL <- "http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz"
    download.file(dataURL, destfile = file.path(destdir, basename(dataURL)))
    untar(file.path(destdir, basename(dataURL)), exdir = destdir)


    ## --------------------------------------------------
    ## Helper functions
    ## --------------------------------------------------
    preprocess_data <- function(dpath) {
        aclimdb_clean(DirSource(dpath, encoding = "UTF-8"))
    }

    make_sparse_mat <- function(dtm) {
        Matrix::sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v)
    }

    make_dtm <- function(data, N = NULL) {

        dtm_data <- DocumentTermMatrix(data)

        ## Make a sparse matrix
        M <- make_sparse_mat(dtm_data)

        ## Find N most frequent terms
        cs <- colSums(M)
        ind <- order(cs, decreasing = TRUE)
        if (! is.null(N))
            colnames(dtm_data)[ind][1:N]
        else
            colnames(dtm_data)[ind]
    }

    make_dictionary <- function(data_pos, data_neg, N) {
        list("common" = make_dtm(c(data_pos, data_neg), N))
    }

    discretize_documents <- function(data, dic) {
        DocumentTermMatrix(data, control = list(dictionary = dic))
    }

    make_dataset <- function(data_pos = NULL, data_neg = NULL, dir_pos = NULL, dir_neg = NULL, dic) {
        if (! (is.null(dir_pos) & is.null(dir_neg))) {
            data_pos <- preprocess_data(dir_pos)
            data_neg <- preprocess_data(dir_neg)
        }

        dataset <- rbind(
            as.matrix(discretize_documents(data_pos, dic = unlist(dic))),
            as.matrix(discretize_documents(data_neg, dic = unlist(dic))))

        dataset <- dataset / apply(dataset, 1, max)
        dataset[is.na(dataset)] <- 0

        dataset <- as.data.frame(dataset)

        list("data" = dataset, "class" = as.factor(c(rep(c("pos", "neg"), each=length(data_neg)))))
    }

    get_acc <- function(model, dataset_test) {
        res <- predict(model, newdata = dataset_test)
        sum(as.character(dataset_test$class) == as.character(res)) / nrow(dataset_test)
    }


    ## --------------------------------------------------
    ## Define directories with the data
    ## --------------------------------------------------
    dir_train_pos <- file.path(destdir, "aclImdb/train/pos/")
    dir_train_neg <- file.path(destdir, "aclImdb/train/neg/")

    dir_test_pos <- file.path(destdir, "aclImdb/test/pos/")
    dir_test_neg <- file.path(destdir, "aclImdb/test/neg/")

    data_train_pos <- preprocess_data(dir_train_pos)
    data_train_neg <- preprocess_data(dir_train_neg)


    ## --------------------------------------------------
    ## (1) Create a dictionary with N words
    ## (2) Make datasets
    ## (3) Create classifiers
    ## (4) Save
    ## --------------------------------------------------

    cat("Processing Data\n")
    N_words <- 1000
    dict    <- make_dictionary(data_pos = data_train_pos, data_neg = data_train_neg, N = N_words)
    dataset_train <- make_dataset(data_pos = data_train_pos, data_neg = data_train_neg, dic = dict)
    dataset_test  <- make_dataset(dir_pos = dir_test_pos, dir_neg = dir_test_neg, dic = dict)

    ## Create models
    cat("Training SVM\n")
    model_svm <- svm(x = dataset_train$data, y = dataset_train$class, kernel = "radial", probability = TRUE)
    saveRDS(model_svm, file.path(destdir, "aclimdb_model_svm.rds"), compress = "xz")

    cat("Training ELM\n")
    model_elm <- elm_train(as.matrix(dataset_train$data), onehot_encode(as.numeric(dataset_train$class) - 1),
        nhid = 1000, actfun = "sig", init_weights = "uniform_negative", bias = TRUE, verbose = TRUE)
    saveRDS(model_elm, file.path(destdir, "aclimdb_model_elm.rds"), compress = "xz")

    cat("Training RF\n")
    model_rf  <- randomForest(x = dataset_train$data, y = dataset_train$class)
    saveRDS(model_rf, file.path(destdir, "aclimdb_model_rf.rds"), compress = "xz")

    cat("Training LogReg\n")
    model_lr <- glm(dataset_train$class ~ ., dataset_train$data, family="binomial", model=FALSE)
    model_lr[c("data", "y", "model", "residuals", "weights", "fitted.values",
        "prior.weights", "na.action", "linear.predictors", "effects", "R")] <- NULL
    model_lr$qr$qr <- NULL
    saveRDS(model_lr, file.path(destdir, "aclimdb_model_lr.rds"), compress = "xz")

    ## Functions for making predictions
    pred_svm <- function(model, data) {
        p_svm <- predict(model, newdata = data, probability = TRUE)
        unname(attr(p_svm, "probabilities")[, 1])
    }

    pred_elm <- function(model, data) {
        p_elm <- elm_predict(model, newdata = as.matrix(data), normalize = TRUE)
        p_elm[, 2]
    }

    pred_rf <- function(model, data) {
        p_rf  <- predict(model, newdata = data, type = "prob")
        unname(p_rf[, 2])
    }

    pred_lr <- function(model, data) {
        p_lr <- predict(model, newdata = data)
        sigmoid(unname(p_lr))
    }


    ## Probability of item being positive
    cat("Predicting Training Data\n")
    dataset_train$prob_svm <- pred_svm(model_svm, dataset_train$data)
    dataset_train$prob_elm <- pred_elm(model_elm, dataset_train$data)
    dataset_train$prob_rf  <- pred_rf(model_rf, dataset_train$data)
    dataset_train$prob_lr  <- pred_lr(model_lr, dataset_train$data)
    saveRDS(dataset_train, file.path(destdir, "aclimdb_data_train.rds"), compress = "xz")

    cat("Predicting Test Data\n")
    dataset_test$prob_svm  <- pred_svm(model_svm, dataset_test$data)
    dataset_test$prob_elm  <- pred_elm(model_elm, dataset_test$data)
    dataset_test$prob_rf   <- pred_rf(model_rf, dataset_test$data)
    dataset_test$prob_lr   <- pred_lr(model_lr, dataset_test$data)
    saveRDS(dataset_test, file.path(destdir, "aclimdb_data_test.rds"), compress = "xz")

    ## --------------------------------------------------
}
