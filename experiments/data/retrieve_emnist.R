## --------------------------------------------------
## Retrieve and preprocess EMNIST
##
##    https://www.westernsydney.edu.au/bens/home/reproducible_research/emnist
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla retrieve_emnist.R destdir
##
## This will download the data, preprocess it
## and create an rds-file containing an image
## matrix (40 000 X 784) and a label vector
## (40 000 X 1).
##
## The argument "destdir" specifies the destination directory
## --------------------------------------------------

library(keras)

train_model <- function(X, Y, smoothing=0.1, epochs=1) {
    shuff <- sample.int(length(Y))
    X <- X[shuff, ]
    Y <- Y[shuff]
    # Label Smoothing
    Y <- to_categorical(Y)
    Y <- Y * (1 - smoothing) + smoothing / ncol(Y)
    n_class <- ncol(Y)
    # Neural Network
    model <- keras_model_sequential()
    model %>%
        layer_reshape(list(28, 28, 1), list(784)) %>%
        #Conv 1
        layer_conv_2d(filters = 8, kernel_size=3, activation = "relu", padding="same") %>%
        layer_batch_normalization() %>%
        layer_max_pooling_2d() %>%
        # Conv 2
        layer_conv_2d(filters = 16, kernel_size=3, activation = "relu", padding="same") %>%
        # layer_batch_normalization() %>%
        layer_max_pooling_2d() %>%
        # Dense
        layer_flatten(input_shape = c(7, 7)) %>%
        layer_dense(units = 32, activation = "relu") %>%
        # layer_dropout(rate = 0.3) %>%
        # Output
        layer_dense(units = n_class, activation = "softmax")
    model %>% compile(
        optimizer = "adam",
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )
    # Train
    model %>% fit(X, Y, epochs = epochs, batch_size = 512, validation_split = 0.2)
    invisible(model)
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
    dir.create(file.path(destdir), showWarnings = FALSE)

    ## --------------------------------------------------
    ## Define Paths
    ## --------------------------------------------------
    dataURL <- "https://www.itl.nist.gov/iaui/vip/cs_links/EMNIST/matlab.zip"
    zipPath <- file.path(destdir, "emnist.zip")
    matPath <- file.path(destdir, "emnist-digits.mat")
    rdsPath <- file.path(destdir, "emnist.rds")
    highPath <- file.path(destdir, "emnist_high")
    evenPath <- file.path(destdir, "emnist_even")
    digiPath <- file.path(destdir, "emnist_digits")

    ## --------------------------------------------------
    ## Retrieve the data
    ## --------------------------------------------------
    download.file(dataURL, destfile=zipPath)
    unzip(zipPath, "matlab/emnist-digits.mat", exdir=destdir, junkpaths=TRUE)

    ## --------------------------------------------------
    ## Save the data
    ## --------------------------------------------------
    library(R.matlab)
    emnist <- readMat(matPath)
    train <- emnist$dataset[[1]]
    train_image <- train[[1]] / 127.5 - 1
    train_label <- train[[2]]
    test <- emnist$dataset[[2]]
    image <- test[[1]] / 127.5 - 1 # 40 000 X 784 (28^2)
    label <- test[[2]] # 40 000 X 1

    saveRDS(list(image=image, label=label), rdsPath, compress = "xz")

    ## --------------------------------------------------
    ## Train Models
    ## --------------------------------------------------
    install_keras()

    print("EMNIST High-Low")
    model <- train_model(train_image, train_label > 4)
    save_model_hdf5(model, paste0(highPath, ".hdf5"))
    saveRDS(predict(model, image)[, 2], paste0(highPath, ".rds"), compress="xz")
    print("EMNIST Even-Odd")
    model <- train_model(train_image, (train_label %% 2) == 0)
    save_model_hdf5(model, paste0(evenPath, ".hdf5"))
    saveRDS(predict(model, image)[, 2], paste0(evenPath, ".rds"), compress="xz")
    print("EMNIST Digits")
    model <- train_model(train_image, train_label)
    save_model_hdf5(model, paste0(digiPath, ".hdf5"))
    saveRDS(predict(model, image), paste0(digiPath, ".rds"), compress="xz")
}
