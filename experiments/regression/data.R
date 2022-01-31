# This script contains methods for downloading and loading datasets

DATA_DIR <- "experiments/data"

.data_download <- function(filename, url, path = DATA_DIR) {
    filepath <- file.path(path, filename)
    if (!file.exists(filepath)) {
        dir.create(path, showWarnings = FALSE)
        download.file(url, filepath)
    }
    filepath
}

.data_exists <- function(filename, path = DATA_DIR) {
    file.exists(file.path(path, filename))
}

# Create a list for the dataset with automatically scaled lambdas
.datacontainer <- function(X, Y, name, epsilon_lg, epsilon_md, epsilon_sm, lambda = 1e-4, ...) {
    list(
        X = X,
        Y = Y,
        name = name,
        epsilon_lg = epsilon_lg,
        epsilon_md = epsilon_md,
        epsilon_sm = epsilon_sm,
        lambda = lambda,
        lambda_lg = lambda * nrow(X) * epsilon_lg^2,
        lambda_md = lambda * nrow(X) * epsilon_md^2,
        lambda_sm = lambda * nrow(X) * epsilon_sm^2,
        ...
    )
}


# Load the Forest Fires (regression) dataset.
# For more information see: https://archive.ics.uci.edu/ml/datasets/Forest+Fires
# This is the eleventh most popular dataset on the UCI ML repository.
#
# The original paper can be found here: http://www3.dsi.uminho.pt/pcortez/fires.pdf
# In the paper they note that a linear model mostly finds the "mean", meaning that the dataset
#   is "too difficult" for a linear model.
# One option would be to ignore most items with area=0 (but that is not the original problem!).
#   This does not fix OLS, but SLISE with small epsilons start working.
# One odd thing is that lower temperatures corresponds to worse fires, but this can be
#   explained with worse fires during the winter (why?).
# Another odd thing is that rain makes the fires worse, but that this effect is the first to
#   disappear when regularisation is added (overfitting?).
data_forest_fires <- function(raw = FALSE, balance = TRUE, months = TRUE, days = TRUE, coord = FALSE, ...) {
    data <- read.csv(.data_download(
        "forest_fires.csv",
        "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
    ))
    if (balance) {
        mask <- which(data$area > 0)
        mask <- c(sample(which(data$area == 0), length(mask) / 9), mask)
        data <- data[sort(mask), ]
    }
    if (raw) {
        data
    } else {
        if (months) {
            data$month <- factor(data$month, tolower(month.abb), month.name)
        } else {
            data$month <- NULL
        }
        if (days) {
            data$day <- factor(
                data$day,
                c("mon", "tue", "wed", "thu", "fri", "sat", "sun"),
                c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
            )
        } else {
            data$day <- NULL
        }
        if (!coord) {
            data$X <- data$Y <- NULL
        }
        .datacontainer(
            model.matrix(area ~ 1 + ., data)[, -1],
            log(data$area + 1),
            "Forest Fires",
            0.1, 0.1, 0.1
        )
    }
}

# Load the Wine Quality (regression) dataset
# For more information see: https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# This is the sixth most popular data set on the UCI ML repository.
#
# Since the targets are (discrete) integers, make sure that epsilon is large enough to "bridge the gaps".
# The different variables have widely different scales, so normalisation is recommended for interpretation.
#
# The original paper can be found here: https://doi.org/10.1016/j.dss.2009.05.016
# After a quick test, SLISE performs better than the linear regression reported
#   in the paper, but slightly worse than the SVM reported in the paper.
# In the paper they separated red and white wine, but combining them causes no issues.
data_wine_quality <- function(type = "all", ...) {
    type <- tolower(type)
    if (type == "all" || type == "red") {
        path_red <- .data_download(
            "wine_quality_red.csv",
            "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
        )
        red <- read.csv(path_red, header = TRUE, sep = ";")
    }
    if (type == "all" || type == "white") {
        path_white <- .data_download(
            "wine_quality_white.csv",
            "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
        )
        white <- read.csv(path_white, header = TRUE, sep = ";")
    }
    if (type == "all") {
        red$type <- factor("red", c("red", "white"))
        white$type <- factor("white", c("red", "white"))
        data <- rbind(red, white)
    } else if (type == "red") {
        data <- red
    } else if (type == "white") {
        data <- white
    } else {
        stop("Unknown type of wine")
    }
    .datacontainer(
        X = model.matrix(quality ~ 1 + ., data)[, -1],
        Y = data$quality,
        name = "Wine Quality",
        epsilon_lg = 0.51,
        epsilon_md = 0.27,
        epsilon_sm = 0.14
    )
}

# Load the Student Performance (regression) dataset
# For more information see: https://archive.ics.uci.edu/ml/datasets/Student+Performance
# This is the twelth most popular data set on the UCI ML repository.
#
# Since the grades for period 3 (the target) are very correlated with the grades for
#   period 1 and 2 I have chosen to remove those.
#
# The original paper can be found here: http://www3.dsi.uminho.pt/pcortez/student.pdf
# After a quick test, SLISE performs similar to, and sometimes better than, the classifiers
#   in the paper (even though SLISE is used for regression).
# Comparing to the regression methods is difficult, since they only report RMSE, which does
#   not take into account that SLISE ignores some items.
data_student_performance <- function(subject = "both", ...) {
    subject <- tolower(subject)
    path <- .data_download(
        "student_performance.zip",
        "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
    )
    if (subject == "maths" || subject == "maths" || subject == "mat") {
        data <- read.csv(unz(path, "student-mat.csv"), sep = ";")
    } else if (subject == "portuguese" || subject == "por") {
        data <- read.csv(unz(path, "student-por.csv"), sep = ";")
    } else if (subject == "both") {
        data <- rbind(
            read.csv(unz(path, "student-mat.csv"), sep = ";"),
            read.csv(unz(path, "student-por.csv"), sep = ";")
        )
    } else {
        stop("Unknown subject")
    }
    .datacontainer(
        X = model.matrix(G3 ~ 1 + . - G2 - G1, data)[, -1],
        Y = data$G3,
        name = "Student",
        epsilon_lg = 0.90,
        epsilon_md = 0.45,
        epsilon_sm = 0.22
    )
}

# Load the Superconductivty dataset
# For more information see: https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data
#
# The original paper can be found here: https://doi.org/10.1016/j.commatsci.2018.07.052
data_superconductivity <- function(n = 10000, ...) {
    path <- .data_download(
        "superconductivity.zip",
        "https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip"
    )
    data <- read.csv(unz(path, "train.csv"))
    sel <- sample.int(nrow(data), min(n, nrow(data)))
    .datacontainer(
        X = model.matrix(critical_temp ~ 1 + ., data)[sel, -1],
        Y = data$critical_temp[sel],
        name = "Superconductivity",
        epsilon_lg = 0.70,
        epsilon_md = 0.30,
        epsilon_sm = 0.14
    )
}


# Load the Air Quality dataset
# For more information see: https://archive.ics.uci.edu/ml/datasets/Air+Quality
#
# The original paper can be found here: http://dx.doi.org/10.1016/j.snb.2007.09.060
# This dataset has also been used in the drifter paper
data_air_quality <- function(...) {
    path <- .data_download(
        "air_quality.zip",
        "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip"
    )
    data <- read.csv2(unz(path, "AirQualityUCI.csv"))
    data <- data[, -c(1, 2, 5, ncol(data), ncol(data) - 1)]
    Y <- data[, 1]
    X <- as.matrix(data[, -1])
    X[X == -200] <- NA
    Y[Y == -200] <- NA
    mask <- complete.cases(Y) & complete.cases(X)
    .datacontainer(
        X = X[mask, ],
        Y = Y[mask],
        name = "Air Quality",
        epsilon_lg = 0.24,
        epsilon_md = 0.12,
        epsilon_sm = 0.07
    )
}


#' Get Physics "jets" data
#'
#' @param n number of jets
#'
#' @return list(X, Y, R=real_class, model)
#'
data_jets <- function(n = 10000, ...) {
    path <- file.path(DATA_DIR, "jets.rds")
    if (!file.exists(path)) {
        warning("Using the data from the conference supplements instead of running `experiments/data/retrieve_jets.R`")
        supp_path <- .data_download(
            "supplement.zip",
            "https://github.com/edahelsinki/slise/releases/download/v1.0/supplement.zip"
        )
        path <- unzip(supp_path, path)
    }
    data <- readRDS(path)
    if (n > 0) {
        mask <- sample(c(sample(which(data$R == 1), n / 2), sample(which(data$R == 0), n / 2)))
        data$R <- data$R[mask]
        data$Y <- data$Y[mask]
        data$X <- data$X[mask, ]
    }
    .datacontainer(
        X = data$X,
        Y = limited_logit(data$Y),
        R = data$R,
        name = "Physics",
        epsilon_lg = 0.33,
        epsilon_md = 0.17,
        epsilon_sm = 0.09
    )
}

#' Get imdb review data
#'
#' @param n number of reviews
#' @param model classifier (svm, rf, lr, elm)
#'
#' @return list(X, Y, R=real_class, model)
#'
data_imdb <- function(n = 10000, model = "svm", ...) {
    path_data <- file.path(DATA_DIR, "aclimdb_data_test.rds")
    path_model <- file.path(DATA_DIR, paste0("aclimdb_model_", model, ".rds"))
    if (!file.exists(path_data) || !file.exists(path_model)) {
        warning("Using the data from the conference supplements instead of running `experiments/data/retrieve_aclimdb.R`")
        supp_path <- .data_download(
            "supplement.zip",
            "https://github.com/edahelsinki/slise/releases/download/v1.0/supplement.zip"
        )
        path_data <- unzip(supp_path, path_data)
        path_model <- unzip(supp_path, path_model)
    }
    tmp <- readRDS(path_data)
    X <- as.matrix(tmp$data)
    R <- as.numeric(tmp$class == "pos")
    Y <- as.numeric(tmp[[paste0("prob_", model)]])
    m <- readRDS(path_model)
    class(m) <- c(model, class(m))
    if (n > 0 && n < length(Y)) {
        mask <- sample.int(length(Y), n)
        X <- X[mask, ]
        Y <- Y[mask]
        R <- R[mask]
    }
    .datacontainer(
        X = X,
        Y = limited_logit(Y),
        R = R,
        name = "IMDB",
        epsilon_lg = 0.29,
        epsilon_md = 0.11,
        epsilon_sm = 0.05
    )
}


# Get EMNIST data
# Params:
#   n:          number of images
#   d:          number of dimensions (default == -1 == all dimensions)
#   digit:      the digit for the "correct" class
#   th:         discard dimensions with variance less than th
#   balance:    should the number of samples in each class be balanced
data_emnist <- function(n = 10000, d = -1, digit = 2, th = 0, balance = TRUE, digit_in_name = FALSE, ...) {
    path_img <- file.path(DATA_DIR, "emnist.rds")
    path_pred <- file.path(DATA_DIR, "emnist_digits.rds")
    if (!file.exists(path_img) || !file.exists(path_pred)) {
        warning("Using the data from the conference supplements instead of running `experiments/data/retrieve_jets.R`")
        supp_path <- .data_download(
            "supplement.zip",
            "https://github.com/edahelsinki/slise/releases/download/v1.0/supplement.zip"
        )
        path_img <- unzip(supp_path, path_img)
        path_pred <- unzip(supp_path, path_pred)
    }
    emnist <- readRDS(path_img)
    X <- emnist$image
    R <- emnist$label
    # Adjusting for softmax
    Y <- readRDS(path_pred)
    Y2 <- apply(Y[, -digit - 1], 1, max)
    Y <- Y[, digit + 1]
    Y <- Y / (Y + Y2)
    # Balancing classes
    if (balance) {
        mask1 <- which(R == digit)
        mask2 <- which(R != digit)
        n <- min(length(mask1), length(mask2), if (n > 0) n / 2 else length(Y))
        mask <- sample(c(sample(mask1, n), sample(mask2, n)))
        Y <- Y[mask]
        X <- X[mask, , drop = FALSE]
        R <- R[mask]
    } else if (n > 0 && n < length(Y)) {
        mask <- sample.int(length(Y), n)
        X <- X[mask, , drop = FALSE]
        Y <- Y[mask]
        R <- R[mask]
    }
    colnames(X) <- paste0("pixel_", 1:ncol(X))
    if (th >= 0) {
        X <- X[, apply(X, 2, var, na.rm = TRUE) > th, drop = FALSE]
    }
    if (d > 0 && d < ncol(X)) {
        X <- X[, sort(sample.int(ncol(X), d)), drop = FALSE]
    }
    .datacontainer(
        X = X,
        Y = limited_logit(Y),
        R = R,
        name = if (digit_in_name) paste("EMNIST", digit) else "EMNIST",
        epsilon_lg = 0.28,
        epsilon_md = 0.13,
        epsilon_sm = 0.05
    )
}

#' Create Synthetic data
#'
#' @param n number of items
#' @param d number of columns
#' @param num_zero number of irrelevant features
#' @param epsilon noise scale
#' @param extra_models number of "wrong" models
#' @param sparsity how large fraction of the coefficients should be zero
#'
#' @return list(X, Y, alpha, clean)
#'
data_create <- function(n = 1000, d = 50, epsilon = 1.0, extra_models = 6, sparsity = 0.0, ...) {
    X <- do.call(rbind, lapply(1:10, function(i) {
          t(matrix(rnorm(n * d / 10, rnorm(d) * 2, runif(d)), nrow = d))
      }))[c(sapply(1:10, seq, n, 10)), ]
    Y <- rep(0, n)
    start <- 1
    if (extra_models > 0) {
        for (i in 1:extra_models) {
            end <- start + n / 10 - 1
            alpha <- runif(d + 1, -1, 1)
            Y[start:end] <- X[start:end, ] %*% alpha[-1] + alpha[1] + runif(n / 10, 0, epsilon)
            start <- end + 1
        }
    }
    alpha <- runif(d + 1, -1, 1)
    sparsity <- round(d * sparsity)
    if (sparsity > 0) {
        alpha[which_min_n(abs(alpha[-1]), sparsity) + 1] <- 0
    }
    Y[start:n] <- X[start:n, ] %*% alpha[-1] + alpha[1] + runif(n - start + 1, 0, epsilon)
    .datacontainer(
        X = X,
        Y = Y,
        alpha = alpha,
        name = "Synthetic",
        epsilon_lg = 0.79,
        epsilon_md = 0.23,
        epsilon_sm = 0.11
    )
}

#' Create Synthetic data that tries to confuse lasso, ols, and zeros initialisation.
#' However, that goal does not work sometimes (with this setup).
#'
#' @param n number of items
#' @param d number of columns
#' @param num_zero number of irrelevant features
#' @param epsilon noise scale
#' @param clean the fraction of clean y:s
#'
#' @return list(X, Y, alpha, clean)
#'
data_create2 <- function(n = 1000, d = 50, epsilon = 1.0, clean = 0.5, ...) {
    X <- do.call(rbind, lapply(1:10, function(i) {
          t(matrix(rnorm(n * d / 10, rnorm(d) * 3, runif(d)), nrow = d))
      }))[c(sapply(1:10, seq, n, 10)), ]
    alpha <- runif(d + 1, -1, 1)
    Y <- X %*% alpha[-1] + alpha[1] + runif(n, 0, epsilon)
    start <- as.integer(clean * n)
    left <- n - start
    if (left > 0) {
        end <- start + as.integer(left * 0.5)
        start <- start + 1
        Y[start:end] <- 0
        start <- end + 1
        may <- max(abs(Y))
        Xi <- add_intercept_column(X)
        for (i in start:n) {
            lm <- .lm.fit(Xi[1:(i - 1), ], Y[1:(i - 1)])
            yi <- sum(Xi[i, ] * lm$coefficients)
            if (yi > 0) {
                Y[i] <- -runif(1, 0, may)
            } else if (yi < 0) {
                Y[i] <- runif(1, 0, may)
            }
        }
    }
    .datacontainer(
        X = X,
        Y = Y,
        alpha = alpha,
        name = "Synthetic",
        epsilon_lg = 0.79,
        epsilon_md = 0.23,
        epsilon_sm = 0.11
    )
}

robust_data <- function(data, scale_x = TRUE, scale_y = TRUE) {
    data2 <- rlang::duplicate(data, shallow = TRUE)
    if (scale_x) {
        data2$X <- scale_robust(remove_constant_columns(data$X))
    } else {
        data2$X <- remove_constant_columns(data$X)
    }
    if (scale_y) {
        data2$Y <- scale_robust(data$Y)
    }
    data2
}

outlier_data <- function(data, vertical = 0.25, leverage = 0.25) {
    n <- length(data$Y)
    rnd <- sample.int(n)
    X <- data$X[rnd, ]
    Y <- data$Y[rnd]
    start <- 1
    vertical <- round(n * vertical)
    if (vertical > 0) {
        sel <- start:(start + vertical - 1)
        start <- start + vertical
        Y[sel] <- rnorm(vertical, 10, 1)
    }
    leverage <- round(n * leverage)
    if (leverage > 0) {
        sel <- start:(start + leverage - 1)
        start <- start + leverage
        X[sel, ] <- rnorm(ncol(X) * leverage, 10, 1)
    }
    data2 <- rlang::duplicate(data, shallow = TRUE)
    data2$X <- X
    data2$Y <- Y
    data2
}

data_at_index <- function(index, ..., seed_start = 42, normalise = TRUE) {
    set.seed(seed_start + floor((index - 1) / 17) + 1)
    index <- (index - 1) %% 17 + 1
    data <- switch(index,
        `1` = data_student_performance(...),
        `2` = data_air_quality(...),
        `3` = data_superconductivity(...),
        `4` = data_jets(...),
        `5` = data_imdb(...),
        `6` = data_emnist(digit = 0, ...),
        `7` = data_emnist(digit = 1, ...),
        `8` = data_emnist(digit = 2, ...),
        `9` = data_emnist(digit = 3, ...),
        `10` = data_emnist(digit = 4, ...),
        `11` = data_emnist(digit = 5, ...),
        `12` = data_emnist(digit = 6, ...),
        `13` = data_emnist(digit = 7, ...),
        `14` = data_emnist(digit = 8, ...),
        `15` = data_emnist(digit = 9, ...),
        `16` = data_create(n = 2000, d = 50, ...),
        `17` = data_create(n = 1000, d = 20, ...)
    )
    if (normalise) {
        data <- robust_data(data, !(index %in% c(5, 7:16)), TRUE)
    }
    data
}

data_all <- function(indices = 1:17, ...) {
    lapply(indices, data_at_index, ...)
}

latex_name <- function(data_name) {
    switch(data_name,
        "Wine Quality" = "\\winequality",
        "Student" = "\\student",
        "Air Quality" = "\\airquality",
        "Superconductivity" = "\\superconductivity",
        "Physics" = "\\physics",
        "IMDB" = "\\imdb",
        "EMNIST" = "\\emnist",
        "Synthetic" = "\\synthetic",
        data_name
    )
}

data_name_factor <- function() {
    names <- c(
        "Air Quality",
        "Student",
        "Superconductivity",
        "Synthetic",
        "Physics",
        "EMNIST",
        "IMDB"
    )
    factor(names, names)
}

## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    devtools::load_all()
    cat("Preparing all datasets...")
    tmpo <- data_all()
    cat("Done!")
}
