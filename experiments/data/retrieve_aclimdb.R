## --------------------------------------------------
## Retrieve and preprocess the
##    "Large Movie Review Dataset"
##
##    http://ai.stanford.edu/~amaas/data/sentiment/
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla retrieve_aclimdb.R [destdir]
##
## This will download the data, preprocess it and create an
## rds-file containing training and testing data.
##
## The argument 'destdir' specifies where the data and models
## should be saved (default: "experiments/data").
##
## Alternatively it can be sourced from a script to only get
## access to the get_data and get_review functions.
##
## --------------------------------------------------


library(stringr)


read_text_file <- function(path) {
    file <- file(path, encoding = "UTF-8")
    str <- readChar(path, file.info(path)$size)
    close(file)
    str_replace_all(str, "((<br />)|(\\n)|(\\\\n))", "\n")
}

aclimdb_get_data <- function(set = "test", datadir = "experiments/data") {
    readRDS(file.path(datadir, paste0("aclimdb_data_", set, ".rds")))
}

aclimdb_get_str <- function(set = "test", datadir = "experiments/data") {
    str <- readRDS(file.path(datadir, paste0("aclimdb_str_", set, ".rds")))
    c(str$pos, str$neg)
}

aclimdb_get_review <- function(index, data = NULL, str = NULL, set = "test", datadir = "experiments/data") {
    if (missing(data)) {
        data <- aclimdb_get_data(set, datadir = datadir)
    }
    if (missing(str)) {
        str <- aclimbd_get_str(set, datadir)
    }

    list(
        x = data$data[index, ],
        y = data$class[index],
        svm = data$prob_svm[index],
        elm = data$prob_elm[index],
        rf = data$prob_rf[index],
        str = str[[index]],
    )
}

aclimdb_get_model <- function(model = "svm", datadir = "experiments/data") {
    if (model == "svm") {
        suppressMessages(library(e1071))
        model <- readRDS(file.path(datadir, paste0("aclimdb_model_", model, ".rds")))
        function(data, ...) {
            p_svm <- predict(model, newdata = data, probability = TRUE, ...)
            unname(attr(p_svm, "probabilities"))[, 2:1, drop = FALSE]
        }
    } else if (model == "elm") {
        suppressMessages(library(elmNNRcpp))
        model <- readRDS(file.path(datadir, paste0("aclimdb_model_", model, ".rds")))
        function(data, ...) {
            unname(elm_predict(model, newdata = as.matrix(data), normalize = TRUE, ...))[, 2:1, drop = FALSE]
        }
    } else if (model == "rf") {
        suppressMessages(library(randomForest))
        model <- readRDS(file.path(datadir, paste0("aclimdb_model_", model, ".rds")))
        function(data, ...) {
            unname(predict(model, newdata = data, type = "prob", ...))[, 2:1, drop = FALSE]
        }
    } else {
        stop("Unknown model")
    }
}

## --------------------------------------------------
## Helper functions
## --------------------------------------------------

aclimdb_clean <- function(source) {
    suppressMessages(library(tm))
    suppressMessages(library(SnowballC))
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

aclimdb_select_words <- function(data, num_words = NULL) {
    suppressMessages(library(tm))
    suppressMessages(library(Matrix))
    dtm_data <- DocumentTermMatrix(data)

    ## Find N most frequent terms
    mat <- sparseMatrix(i = dtm_data$i, j = dtm_data$j, x = dtm_data$v)
    cs <- colSums(mat)
    ind <- order(cs, decreasing = TRUE)
    if (!is.null(num_words)) {
        colnames(dtm_data)[ind[1:num_words]]
    } else {
        colnames(dtm_data)[ind]
    }
}

aclimdb_make_dataset <- function(data_pos, data_neg, words) {
    suppressMessages(library(tm))
    dataset <- rbind(
        as.matrix(DocumentTermMatrix(data_pos, control = list(dictionary = words))),
        as.matrix(DocumentTermMatrix(data_neg, control = list(dictionary = words)))
    )
    dataset <- sweep(dataset, 1, apply(dataset, 1, max) + 0.1, `/`)
    classes <- factor(c("pos", "neg"))

    list(
        "data" = as.data.frame(dataset),
        "class" = classes[c(rep(1, length(data_pos)), rep(2, length(data_neg)))]
    )
}


# This is only run when called from Rscript
if (sys.nframe() == 0L) {
    ## --------------------------------------------------
    ## Libraries
    ## --------------------------------------------------
    library(tm)
    library(Matrix)
    library(SnowballC)
    library(e1071)
    library(randomForest)
    library(elmNNRcpp)

    ## --------------------------------------------------
    ## Command-line arguments
    ## --------------------------------------------------
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 1) {
        destdir <- "experiments/data"
    } else {
        destdir <- args[1]
    }
    dir.create(file.path(destdir), showWarnings = FALSE)


    ## --------------------------------------------------
    ## Paths
    ## --------------------------------------------------
    data_url <- "http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz"
    path_data <- file.path(destdir, basename(data_url))
    path_str_test <- file.path(destdir, "aclimdb_str_test.rds")
    path_str_train <- file.path(destdir, "aclimdb_str_train.rds")
    path_data_test <- file.path(destdir, "aclimdb_data_test.rds")
    path_data_train <- file.path(destdir, "aclimdb_data_train.rds")
    path_models <- file.path(destdir, "aclimdb_model_")


    ## --------------------------------------------------
    ## Retrieve the data
    ## --------------------------------------------------
    if (!file.exists(path_data)) {
        cat("Downloading Data\n")
        download.file(data_url, destfile = path_data)
    }
    if (!file.exists(path_str_test) || !file.exists(path_str_train)) {
        cat("Extracting Data\n")
        dir <- tempdir()
        untar(path_data, exdir = dir)
        test_str <- list(
            pos = sapply(list.files(file.path(dir, "aclImdb/test/pos"), full.names = TRUE), read_text_file, USE.NAMES = FALSE),
            neg = sapply(list.files(file.path(dir, "aclImdb/test/neg"), full.names = TRUE), read_text_file, USE.NAMES = FALSE)
        )
        saveRDS(test_str, path_str_test, compress = "xz")
        train_str <- list(
            pos = sapply(list.files(file.path(dir, "aclImdb/train/pos"), full.names = TRUE), read_text_file, USE.NAMES = FALSE),
            neg = sapply(list.files(file.path(dir, "aclImdb/train/neg"), full.names = TRUE), read_text_file, USE.NAMES = FALSE)
        )
        saveRDS(train_str, path_str_train, compress = "xz")
        unlink(file.path(dir, "aclImbd"), TRUE)
    } else {
        cat("Loading Data\n")
        test_str <- readRDS(path_str_test)
        train_str <- readRDS(path_str_train)
    }


    ## --------------------------------------------------
    ## Preprocess the data
    ## --------------------------------------------------
    if (!file.exists(path_data_test) || !file.exists(path_data_train)) {
        cat("Processing Data\n")
        test_pos <- aclimdb_clean(VectorSource(test_str$pos))
        test_neg <- aclimdb_clean(VectorSource(test_str$neg))
        train_pos <- aclimdb_clean(VectorSource(train_str$pos))
        train_neg <- aclimdb_clean(VectorSource(train_str$neg))

        num_words <- 1000
        words <- aclimdb_select_words(c(train_pos, train_neg), num_words)
        dataset_train <- aclimdb_make_dataset(train_pos, train_neg, words)
        saveRDS(dataset_train, path_data_train, compress = "xz")
        dataset_test <- aclimdb_make_dataset(test_pos, test_neg, words)
        saveRDS(dataset_test, path_data_test, compress = "xz")
    } else {
        cat("Loading Processed Data\n")
        dataset_train <- readRDS(path_data_train)
        dataset_test <- readRDS(path_data_test)
    }


    ## --------------------------------------------------
    ## Train models
    ## --------------------------------------------------
    path_svm <- paste0(path_models, "svm.rds")
    if (!file.exists(path_svm)) {
        cat("Training SVM\n")
        model_svm <- svm(x = dataset_train$data, y = dataset_train$class, kernel = "radial", probability = TRUE)
        saveRDS(model_svm, path_svm, compress = "xz")
    }
    path_elm <- paste0(path_models, "elm.rds")
    if (!file.exists(path_elm)) {
        cat("Training ELM\n")
        model_elm <- elm_train(as.matrix(dataset_train$data), onehot_encode(as.numeric(dataset_train$class) - 1),
            nhid = 1000, actfun = "sig", init_weights = "uniform_negative", bias = TRUE, verbose = TRUE
        )
        saveRDS(model_elm, path_elm, compress = "xz")
    }
    path_rf <- paste0(path_models, "rf.rds")
    if (!file.exists(path_rf)) {
        cat("Training RF\n")
        model_rf <- randomForest(x = dataset_train$data, y = dataset_train$class)
        saveRDS(model_rf, path_rf, compress = "xz")
    }


    ## --------------------------------------------------
    ## Making predictions
    ## --------------------------------------------------
    pred_svm <- aclimdb_get_model("svm")
    pred_elm <- aclimdb_get_model("elm")
    pred_rf <- aclimdb_get_model("rf")

    cat("Predicting Train Data\n")
    dataset_train$prob_svm <- pred_svm(dataset_train$data)[, 2]
    dataset_train$prob_elm <- pred_elm(dataset_train$data)[, 2]
    dataset_train$prob_rf <- pred_rf(dataset_train$data)[, 2]
    saveRDS(dataset_train, path_data_train, compress = "xz")

    cat("Predicting Test Data\n")
    dataset_test$prob_svm <- pred_svm(dataset_test$data)[, 2]
    dataset_test$prob_elm <- pred_elm(dataset_test$data)[, 2]
    dataset_test$prob_rf <- pred_rf(dataset_test$data)[, 2]
    saveRDS(dataset_test, path_data_test, compress = "xz")
}