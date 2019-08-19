## --------------------------------------------------
## Train models for jets
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla retrieve_jets.R
##
## This will train the jet-classifiers.
##
## --------------------------------------------------

library(keras)

train_model_tab <- function(X, Y, smoothing = 0.1, epochs = 5) {
    shuff1 <- which(Y == 1)
    shuff2 <- which(Y == 0)
    len <- min(length(shuff1), length(shuff2))
    shuff <- sample(c(sample(shuff1, len), sample(shuff2, len)))
    X <- X[shuff, ]
    Y <- Y[shuff]
    # Label Smoothing
    Y <- to_categorical(Y)
    Y <- Y * (1 - smoothing) + smoothing / ncol(Y)
    # Neural Network
    model <- keras_model_sequential()
    model %>%
        layer_batch_normalization(input_shape = 5) %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 2, activation = "softmax")
    model %>% compile(
        optimizer = "adam",
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )
    # Train
    model %>% fit(X, Y, epochs = epochs, batch_size = 128, validation_split = 0.2)
    invisible(model)
}


# This is only run when called from Rscript
if (sys.nframe() == 0L) {

    ## --------------------------------------------------
    ## Define Paths
    ## --------------------------------------------------
    data_tab_in <- "experiments/data/jets.csv"
    data_tab_out <- "experiments/data/jets.rds"
    model_tab_out <- "experiments/data/jets.hdf5"


    ## --------------------------------------------------
    ## Read the data
    ## --------------------------------------------------
    if (file.exists(data_tab_in)) {
        cat("Reading data\n")
        data_tab <- as.matrix(read.csv(data_tab_in))
        label_tab <- data_tab[, 1]
        data_tab <- data_tab[, -1]
    } else if (file.exists(data_tab_out)) {
        cat("Reading data\n")
        data_tab <- readRDS(data_tab_out)
        label_tab <- data_tab$R
        data_tab <- data_tab$X
    } else {
        stop("Jets need to be extracted from a root file first! (see RootParser.cpp)")
    }

    ## --------------------------------------------------
    ## Train Models
    ## --------------------------------------------------
    install_keras()

    cat("Training tabular model\n")
    model_tab <- train_model_tab(data_tab, label_tab)
    cat("Saving tabular model\n")
    save_model_hdf5(model_tab, model_tab_out)
    cat("Predicting jets\n")
    predicted_tab <- predict(model_tab, data_tab)
    cat("Saving tabular data\n")
    saveRDS(list(X = data_tab, Y = predicted_tab[, 2], R = label_tab), data_tab_out, compress = "xz")
}
