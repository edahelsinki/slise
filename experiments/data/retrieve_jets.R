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
library(tensorflow)

jets_train_model_img <- function(X, Y, smoothing = 0.2, epochs = 5) {
    # Neural Network
    model <- keras_model_sequential()
    model %>% # 18 > 14 > 7 > 5 > 3 > Dense
        layer_reshape(list(18, 18, 1), list(18 * 18)) %>%
        layer_locally_connected_2d(filters = 16, kernel_size = 5, activation = "relu", padding = "valid") %>%
        layer_batch_normalization() %>%
        layer_max_pooling_2d() %>%
        layer_locally_connected_2d(filters = 32, kernel_size = 3, activation = "relu", padding = "valid") %>%
        layer_batch_normalization() %>%
        layer_locally_connected_2d(filters = 16, kernel_size = 3, activation = "relu", padding = "valid") %>%
        layer_flatten(input_shape = c(3, 3)) %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_dense(units = 1, activation = "sigmoid")
    model %>% compile(
        optimizer = "adam",
        loss = tf$losses$BinaryCrossentropy(label_smoothing = smoothing),
        metrics = c("accuracy")
    )
    # Train
    model %>% fit(
        X,
        Y,
        epochs = epochs,
        batch_size = 128,
        validation_split = 0.2,
        class_weight = as.list(length(Y) / table(Y))
    )
    model
}

jets_train_model_tab <- function(X, Y, smoothing = 0.2, epochs = 5) {
    # Neural Network
    model <- keras_model_sequential()
    model %>%
        layer_batch_normalization(input_shape = as.integer(ncol(X))) %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "sigmoid")
    model %>% compile(
        optimizer = "adam",
        loss = tf$losses$BinaryCrossentropy(label_smoothing = smoothing),
        metrics = c("accuracy")
    )
    # Train
    model %>% fit(
        X,
        Y,
        epochs = epochs,
        batch_size = 128,
        validation_split = 0.2,
        class_weight = as.list(length(Y) / table(Y))
    )
    model
}

jets_get_data_img <- function() {
    readRDS("experiments/data/jets_img.rds")
}

jets_get_data_tab <- function() {
    readRDS("experiments/data/jets.rds")
}


# This is only run when called from Rscript
if (sys.nframe() == 0L) {

    ## --------------------------------------------------
    ## Define Paths
    ## --------------------------------------------------
    url_data <- "https://hot.hip.fi/tuples/CMSOpenDataJets_QCD_MC_8TeV_500K.root"
    path_root <- "experiments/data/CMSOpenDataJets_MC_8TeV_500K.root"
    path_img_csv <- "experiments/data/jets_img.csv"
    path_img_rds <- "experiments/data/jets_img.rds"
    path_img_model <- "experiments/data/jets_img.hdf5"
    path_tab_csv <- "experiments/data/jets.csv"
    path_tab_rds <- "experiments/data/jets.rds"
    path_tab_model <- "experiments/data/jets.hdf5"


    ## --------------------------------------------------
    ## Retrieve the data
    ## --------------------------------------------------
    if (!file.exists(path_root)) {
        cat("Downloading Data\n")
        download.file(url_data, destfile = path_root)
    }

    if (!file.exists(path_img_rds) || !file.exists(path_tab_rds)) {
        if (!file.exists(path_img_csv) || !file.exists(path_tab_csv)) {
            cat("Extracting data\n")
            # This requries ROOT to be installed: https://root.cern.ch
            system("root -l -b -q experiments/data RootParser.cpp")
        }
        cat("Reading data\n")
        data_tab <- as.matrix(read.csv(path_tab_csv))
        label_tab <- data_tab[, 1]
        data_tab <- data_tab[, -1]
        data_img <- as.matrix(read.csv(path_img_csv))
        label_img <- data_img[, 1]
        data_img <- data_img[, -1]
    } else {
        data_tab <- readRDS(path_tab_rds)
        label_tab <- data_tab$R
        data_tab <- data_tab$X
        data_img <- readRDS(path_img_rds)
        label_img <- data_img$R
        data_img <- data_img$X
    }

    ## --------------------------------------------------
    ## Train Models
    ## --------------------------------------------------
    cat("Training image model\n")
    model_img <- jets_train_model_img(data_img, label_img)
    cat("Saving image model\n")
    save_model_hdf5(model_img, path_img_model)
    cat("Predicting images\n")
    predicted_img <- predict(model_img, data_img)
    cat("Saving image data\n")
    saveRDS(list(X = data_img, Y = predicted_img, R = label_img), path_img_rds, compress = TRUE)

    cat("Training tabular model\n")
    model_tab <- jets_train_model_tab(data_tab, label_tab)
    cat("Saving tabular model\n")
    save_model_hdf5(model_tab, path_tab_model)
    cat("Predicting jets\n")
    predicted_tab <- predict(model_tab, data_tab)
    cat("Saving tabular data\n")
    saveRDS(list(X = data_tab, Y = predicted_tab, R = label_tab), path_tab_rds, compress = TRUE)
}