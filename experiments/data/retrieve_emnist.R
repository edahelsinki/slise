## --------------------------------------------------
## Retrieve and preprocess EMNIST
##
##    https://www.westernsydney.edu.au/bens/home/reproducible_research/emnist
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/data/retrieve_emnist.R [destdir]
##
## This will download the data, preprocess it and create an rds-file containing
## a data matrix (40 000 X 784) and a label vector (40 000 X 1).
##
## The argument 'destdir' specifies where the data and models
## should be saved (default: "experiments/data").
##
## Alternatively it can be sourced from a script to only access the functions.
##
## --------------------------------------------------

library(keras)

emnist_model <- function() {
    model <- keras_model_sequential()
    # Average pooling to avoid noise when doing activation maximisation.
    model %>%
        layer_reshape(c(28, 28, 1), c(784)) %>%
        layer_conv_2d(filters = 16, kernel_size = 3, activation = "relu", padding = "same") %>%
        layer_average_pooling_2d() %>%
        layer_conv_2d(filters = 16, kernel_size = 3, activation = "relu", padding = "valid") %>%
        layer_average_pooling_2d() %>%
        layer_batch_normalization() %>%
        layer_conv_2d(filters = 8, kernel_size = 3, padding = "valid") %>%
        layer_average_pooling_2d() %>%
        layer_flatten(input_shape = c(2, 2), name = "flat") %>%
        layer_activation("relu") %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_dropout(0.5) %>%
        layer_dense(units = 10, activation = "softmax")
    model %>% compile(
        optimizer = "adam",
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )
    model
}

emnist_train <- function(model, X, Y, smoothing = 0.2, epochs = 2) {
    Y <- to_categorical(Y)
    if (smoothing > 0) {
        Y <- Y * (1 - smoothing) + smoothing / ncol(Y)
    }
    model %>% fit(X, Y, epochs = epochs, batch_size = 512, validation_split = 0.2)
}

emnist_get_data <- function(datadir = "experiments/data") {
    data <- readRDS(file.path(datadir, "emnist.rds"))
    data$prediction <- readRDS(file.path(datadir, "emnist_preds.rds"))
    data
}

emnist_get_internal <- function(datadir = "experiments/data") {
    readRDS(file.path(datadir, "emnist_internal.rds"))
}

# Visualise internal workings of a neural network
#  - Activation Maximation
#  - Visualising internal nodes (at the flattening layer)
#  - loosely based on: https://blog.keras.io/how-convolutional-neural-networks-see-the-world.html
activation_maximisation <- function(model, X, node, layer = "flat", iterations = 20, step_size = 0.05, l2 = 100, smooth = TRUE, verbose = TRUE) {
    output_node <- get_layer(model, "flat")$output[, node]

    # Loss that maximises a specific node with regularisation to "dim" irrelevant pixels
    loss <- k_mean(output_node) - l2 * k_mean((model$input - 0.5)^2)
    # Normalised gradients
    grads <- k_gradients(loss, model$input)
    grads <- grads / (k_sqrt(k_mean(k_square(grads), 0, TRUE)) + 1e-5)
    # Gradient ascent
    image <- model$input + grads * step_size # - 0.5
    #  Enforce [0, 1] constraints for the images
    # scale <- k_sqrt(k_maximum(k_max(k_abs(image), 0, TRUE) * 2.0, 1.0))
    # image <- image / scale + 0.5
    image <- k_clip(image, 0.0, 1.0)
    # Create a tensorflow function that can be called iteratively
    image <- k_reshape(image, k_shape(model$input))
    diff <- k_mean(k_abs(image - model$input))
    iterate <- k_function(c(model$input), c(image, diff))

    # Push the images closer to the neutral 0.5
    X <- 0.2 + X * 0.6 # + runif(length(X), -0.1, 0.1)
    diff <- 0
    # Iteratively update the images
    for (i in 1:iterations) {
        out <- iterate(X)
        X <- out[[1]]
        diff <- diff + out[[2]]
    }

    # Optional Gaussian blur (3x3) to make interpretation easier
    if (smooth) {
        kernel_index <- c(-29, -28, -27, -1, 0, 1, 27, 28, 29)
        kernel_weight <- c(1, 2, 1, 2, 4, 2, 1, 2, 1)
        X2 <- X
        for (i in 1:784) {
            index <- i + kernel_index
            mask <- index > 0 & index < 785 & abs(((index - 1) %% 28) - ((i - 1) %% 28)) < 2
            X[, i] <- apply(X2[, index[mask]], 1, weighted.mean, kernel_weight[mask])
        }
    }

    if (verbose) {
        cat(sprintf("Activation Maximisations for node %2d trained! (md: %g)\n", node, diff))
    }
    X
}



# This is only run when called from Rscript
if (sys.nframe() == 0L) {
    suppressPackageStartupMessages(library(R.matlab))

    # This is the easiest way to get k_gradient to work
    suppressPackageStartupMessages(library(tensorflow))
    tf$compat$v1$disable_eager_execution()
    q <- tf$add(1, 2) # Display all TF messages now

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
    ## Define Paths
    ## --------------------------------------------------
    url_data <- "https://www.itl.nist.gov/iaui/vip/cs_links/EMNIST/matlab.zip"
    path_zip <- file.path(destdir, "emnist.zip")
    path_test <- file.path(destdir, "emnist.rds")
    path_model <- file.path(destdir, "emnist_model.hdf5")
    path_pred <- file.path(destdir, "emnist_preds.rds")
    path_internal <- file.path(destdir, "emnist_internal.rds")

    ## --------------------------------------------------
    ## Retrieve the data
    ## --------------------------------------------------
    if (!file.exists(path_zip)) {
        cat("Downloading Data...\n")
        download.file(url_data, destfile = path_zip)
    }

    cat("Reading Data...\n")
    emnist <- readMat(unz(path_zip, "matlab/emnist-digits.mat"))
    train <- emnist$dataset[[1]]
    train_image <- train[[1]] / 255
    train_label <- c(train[[2]])
    test <- emnist$dataset[[2]]
    test_image <- test[[1]] / 255 # 40 000 X 784 (28^2)
    test_label <- c(test[[2]]) # 40 000 X 1
    rm(emnist, train, test)

    if (!file.exists(path_test)) {
        print("Saving Data...\n")
        saveRDS(list(image = test_image, label = test_label), path_test, compress = "xz")
    }

    ## --------------------------------------------------
    ## Train Model
    ## --------------------------------------------------
    if (!file.exists(path_model)) {
        print("Training Model...\n")
        model <- emnist_model()
        emnist_train(model, train_image, train_label)
        print("Saving Model...\n")
        save_model_hdf5(model, path_model)
    } else {
        print("Loading Model...\n")
        model <- load_model_hdf5(path_model)
    }
    if (!file.exists(path_pred)) {
        print("Saving Predictions...\n")
        predictions <- predict(model, test_image)
        saveRDS(predictions, path_pred, compress = "xz")
    }

    ## --------------------------------------------------
    ## Activation Maximisations
    ## --------------------------------------------------
    if (!file.exists(path_internal)) {
        cat("Training Activation Maximisations...\n")
        selected <- seq(100, nrow(test_image), 100) # 30990
        model2 <- keras_model(inputs = model$input, outputs = get_layer(model, "flat")$output)
        internal <- predict(model2, test_image)
        colnames(internal) <- paste("Neuron", 1:ncol(internal))
        nodes <- lapply(1:ncol(internal), function(i) {
            activation_maximisation(model, test_image[selected, , drop = FALSE], i)
        })
        print("Saving AMs...\n")
        saveRDS(list(internal = internal, nodes = nodes, selected = selected), path_internal, compress = "xz")
    }
}