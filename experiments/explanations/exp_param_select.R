
suppressMessages(source("experiments/explanations/utils.R"))
suppressMessages(source("experiments/explanations/data.R"))

suppressMessages(library(tidyr))
suppressMessages(library(dplyr))


cv <- function(X, Y, data, k = 10, l2 = 0.0, path = NULL, verbose = TRUE,
               epsilons = c(0.1, 0.2, 0.4, 0.6, 0.8, 1.0),
               lambdas = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2)) {
    splits <- sample(rep_len(1:k, length(Y)))
    extra_lambdas <- c(data$lambda1 / 2, data$lambda1, data$lambda1 * 2)
    extra_lambdas <- setdiff(extra_lambdas, lambdas)
    df <- expand.grid(
        lambda = c(lambdas, extra_lambdas),
        epsilon = epsilons,
        mse = 0,
        mae = 0,
        loss = 0,
        subset = 0,
        data = factor(data$name)
    )
    last <- (df$epsilon %in% epsilons[c(1, length(epsilons))]) * 1 +
        (df$lambda %in% lambdas[c(1, length(epsilons))]) * 0.5 +
        (df$lambda %in% extra_lambdas) * 0.25
    df <- df[order(last), ]
    if (!is.null(path) && file.exists(path)) {
        df2 <- readRDS(path) %>% filter(epsilon %in% epsilons & lambda %in% lambdas)
        if (nrow(df2) == nrow(df)) {
            return()
        }
        mask <- mapply(function(e, l) !any(e == df2$epsilon & l == df2$lambda), df$epsilon, df$lambda)
        df <- rbind(as.data.frame(df2), df[which(mask), ])
        start <- nrow(df2) + 1
    } else {
        start <- 1
    }
    time <- proc.time()[3] + rnorm(1, 0, 5)
    for (i in start:nrow(df)) {
        epsilon <- df$epsilon[[i]]
        lambda <- df$lambda[[i]]
        R <- c()
        for (s in 1:k) {
            mod <- slise.fit(X[splits != s, ], Y[splits != s], epsilon, lambda, l2 * lambda)
            R <- c(R, (predict(mod, X[splits == s, ]) - Y[splits == s]))
        }
        R2 <- R^2
        df$mse[[i]] <- mean(R2)
        df$mae[[i]] <- mean(abs(R))
        df$loss[[i]] <- loss_sharp_res(NULL, R2, epsilon^2)
        df$subset[[i]] <- mean(R2 <= epsilon^2)
        if (verbose) {
            if (i == 1) {
                cat(sprintf("%8s", names(df)), "\n")
            }
            cat(sprintf("%8s", sprintf("%7.4f", df[i, ])), "\n")
            if (i %% 10 == 0) {
                flush.console()
            }
        }
        if (!is.null(path) && (i == nrow(df) || proc.time()[3] - time > 120)) {
            saveRDS(df[1:i, ], path, compress = "xz")
            time <- proc.time()[3]
        }
    }
    df$data <- factor(df$data)
    df
}

plot_results <- function(df) {
    df$loss_scaled <- df$loss / df$epsilon # / df$subset
    df$epsilon_ <- factor(df$epsilon)
    df$lambda_ <- factor(df$lambda)
    plot(
        ggplot(df %>% pivot_longer(c(mse, mae, subset, loss_scaled))) +
            facet_wrap(vars(name, data), nrow = 4, scales = "free_y") +
            geom_smooth(aes(lambda, value, color = epsilon_, linetype = epsilon_), span = 0.5) +
            scale_x_log10() +
            theme_paper(legend.title = element_text())
    )
    # plot(
    #     ggplot(df %>% pivot_longer(c(mse, loss, subset, loss_scaled))) +
    #         facet_grid(vars(name), vars(epsilon), scales = "free") +
    #         geom_line(aes(lambda, value)) +
    #         scale_x_log10() +
    #         theme_paper()
    # )
}

select_params <- function(df, size = 0.5) {
    df %>%
        group_by(data, epsilon, lambda) %>%
        summarise_all(mean) %>%
        group_by(data, epsilon) %>%
        filter(mae == min(mae)) %>%
        group_by(data) %>%
        filter((subset - size)^2 == min((subset - size)^2))
}

get_data <- function(index) {
    index <- index - 1
    set.seed(42 + index)
    datasets <- list("cars" = 10, "physics" = 10, "imdb" = 20, "jet images" = 10, "emnist" = 10)
    numds <- sum(unlist(datasets))

    iter <- index
    cont <- TRUE
    while (cont) {
        for (i in seq_along(datasets)) {
            if (iter < datasets[[i]]) {
                dataset <- names(datasets)[[i]]
                cont <- FALSE
                break
            }
            iter <- iter - datasets[[i]]
        }
    }

    if (dataset == "physics") {
        data <- data_jets(n = 10000, scale = TRUE)
        data$Y <- limited_logit(data$Y)
    } else if (dataset == "jet images") {
        data <- data_jets(n = 5000, img = TRUE, scale = TRUE)
        # In case it makes things faster
        data$X <- data$X[, apply(data$X, 2, sum) > 0]
        data$Y <- limited_logit(data$Y)
        data$l2 <- 2
    } else if (dataset == "imdb") {
        data <- data_imdb(n = 5000)
        # The explanations will use a limited number of words
        data$X <- data$X[, which(data$X[sample.int(nrow(data$X), 1), ] >= 0)]
        data$Y <- limited_logit(data$Y)
    } else if (dataset == "cars") {
        data <- data_mtcars(scale = TRUE)
    } else if (dataset == "emnist") {
        data <- data_emnist(iter, th = 1e-8)
        data$name2 <- data$name
        data$name <- "emnist"
        data$Y <- limited_logit(data$Y)
        data$l2 <- 2
    } else {
        stop("Other datasets are not implemented yet")
    }
    data$l2 <- ifelse(is.null(data$l2), 0, data$l2)
    data
}



## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        if (length(args) > 1) {
            reticulate::use_python(args[2])
        }
        dir.create("experiments/results/params", showWarnings = FALSE, recursive = TRUE)
        for (job_index in eval(parse(text = args[1]))) {
            suppressMessages(data <- get_data(job_index))
            file <- sprintf("experiments/results/params/params_%03d_%s.rds", job_index, data$name)
            time <- proc.time()[3]
            cat(sprintf("[params] Init %3d: %s\n", job_index, data$name))
            cv(data$X, data$Y, data, verbose = FALSE, l2 = data$l2, path = file)
            cat(sprintf("[params] Done %3d: %s %.1f seconds\n", job_index, data$name, proc.time()[3] - time))
        }
    } else {
        df <- NULL
        for (f in list.files("experiments/results/params", full.names = TRUE)) {
            res <- readRDS(f)
            tryCatch(df <- rbind(df, res), error = function(e) cat("Could not join df:", f, "\n"))
            res <- NULL
        }
        plot_results(df)
        print(select_params(df))
    }
}