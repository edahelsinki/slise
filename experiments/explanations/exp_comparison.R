## --------------------------------------------------
## Compare the explanations from different model-agnostic post-hoc explainers
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/exp_comparison.R index [iter] [python]
##
## Parameters:
##
##        index   : Specify the job index (min=1, max=multiple of 8*11)
##        iter    : Optional number of iterations
##        python  : Optional python executable
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        Run the script without an index to produce plots.
##
## Running Manually:
##        Rscript --vanilla experiments/explanations/exp_comparison.R 1:88 100
##        Rscript --vanilla experiments/explanations/exp_comparison.R
## --------------------------------------------------

suppressMessages(source("experiments/explanations/exp_comparison2.R"))

make_emnist_inverse <- function(X) {
    d <- ncol(X)
    default <- quantile(X[X > 0], 0.9)
    maxX <- apply(X, 2, function(x) max(kmeans(c(default, x), 2)$centers))
    minX <- min(X)
    function(x, coef, impact, pred_fn) {
        if (is.null(coef)) {
            x_inv <- ifelse(abs(x - minX) > abs(x - maxX), pmin(x, minX), pmax(x, maxX))
            noise <- rnorm(length(impact), 0, 1e-8)
            # Negative impacts are destroyed
            xmax <- ifelse(impact >= -1e-8, x, x_inv)
            omax <- order(impact + noise, decreasing = FALSE)
            # Positive impacts are destroyed
            xmin <- ifelse(impact <= 1e-8, x, x_inv)
            omin <- order(impact + noise, decreasing = TRUE)
        } else {
            noise <- rnorm(length(coef), 0, 1e-8)
            xpos <- pmax(maxX, x)
            xneg <- pmin(minX, x)
            pos <- coef * xpos > coef * xneg
            # Positive changes are made
            xmax <- ifelse(abs(coef) < 1e-8, x, ifelse(pos, xpos, xneg))
            omax <- order(xmax * coef - x * coef + noise, decreasing = TRUE)
            # Negative changes are made
            xmin <- ifelse(abs(coef) < 1e-8, x, ifelse(pos, xneg, xpos))
            omin <- order(xmin * coef - x * coef + noise, decreasing = FALSE)
        }
        # print(sapply(auto_named_list(x, coef, impact, xmax, omax, xmin, omin), function(x) any(is.na(x))))
        list("xmax" = xmax, "omax" = omax, "xmin" = xmin, "omin" = omin)
    }
}

get_config <- function(index) {
    set.seed(42 + index)
    reticulate::py_set_seed(as.integer(42 + index))
    methods <- list(
        "slise" = slise_emnist,
        # "slise (distance)" =  distslise_emnist,
        "lime-slise" = limeslise_emnist,
        "shap (grey)" = function(...) shap_emnist(..., deletion = "grey", mid = 0.5),
        # "shap (invert)" = function(...) shap_emnist(..., deletion = "invert", low = 0, mid = 0.5, high = 1),
        "shap (sample)" = function(...) shap_emnist(..., deletion = "sample"),
        "lime (original)" = function(...) lime_emnist(..., segmentation = "original"),
        "lime (small)" = function(...) lime_emnist(..., segmentation = "small"),
        # "lime (pixel)" = function(...) lime_emnist(..., segmentation = "pixel"),
        "global" = global_emnist,
        "random" = rndexpl_emnist
    )
    datasets <- list(
        "emnist0" = function() data_emnist(0, pred_fn = TRUE),
        "emnist1" = function() data_emnist(1, pred_fn = TRUE),
        "emnist2" = function() data_emnist(2, pred_fn = TRUE),
        "emnist3" = function() data_emnist(3, pred_fn = TRUE),
        "emnist4" = function() data_emnist(4, pred_fn = TRUE),
        "emnist5" = function() data_emnist(5, pred_fn = TRUE),
        "emnist6" = function() data_emnist(6, pred_fn = TRUE),
        "emnist7" = function() data_emnist(7, pred_fn = TRUE),
        "emnist8" = function() data_emnist(8, pred_fn = TRUE),
        "emnist9" = function() data_emnist(9, pred_fn = TRUE),
        "jet images" = function() data_jets(img = TRUE, pred_fn = TRUE, scale = TRUE)
    )
    df <- expand.grid(
        method = names(methods),
        dataset = names(datasets)
    )
    index <- ((index - 1) %% nrow(df)) + 1
    data <- datasets[[df$dataset[index]]]()
    data$name <- stringr::str_replace(data$name, " \\d", "")
    data$method <- df$method[index]
    data$expl_fn <- methods[[data$method]]
    data$distance_fn <- cosine_distance
    data$destroy_fn <- make_emnist_inverse(data$X)
    data$times <- ifelse(df$dataset[index] == "jet images", 1, 0.1)
    data
}


## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) > 0) {
        if (length(args) > 1) {
            iterations <- as.integer(args[2])
        } else {
            iterations <- 10
        }
        if (length(args) > 2) {
            reticulate::use_python(args[3])
        }
        dir.create("experiments/results/comparison", showWarnings = FALSE, recursive = TRUE)
        for (job_index in eval(parse(text = args[1]))) {
            suppressMessages(params <- get_config(job_index))
            iterations <- ceiling(iterations * params$times)
            file <- sprintf("experiments/results/comparison/%03d_%s_%s.rds", job_index, params$name, params$method)
            if (!file.exists(file)) {
                df1 <- NULL
                df2 <- NULL
                time <- proc.time()[3]
                time2 <- proc.time()[3]
                cat(sprintf("[comparison] Init %3d: %s %s\n", job_index, params$name, params$method))
                for (i in 1:iterations) {
                    if (proc.time()[3] - time2 > 120) {
                        saveRDS(list(df1 = df1, df2 = df2), file, compress = "xz")
                        time2 <- proc.time()[3]
                    }
                    data <- get_index(params)
                    cat(sprintf("[comparison] Iter %3d: %3d\n", job_index, i))
                    res <- evaluate_expl(data, data$index, iter = i, job = job_index)
                    df1 <- rbind(df1, res$df1)
                    df2 <- rbind(df2, res$df2)
                }
                saveRDS(list(df1 = df1, df2 = df2), file, compress = "xz")
                time <- proc.time()[3] - time
                cat(sprintf("[comparison] Done %3d: %.1f seconds\n", job_index, time))
            } else {
                cat("[comparison] Exists:", job_index, "\n")
            }
        }
    } else {
        df1 <- NULL
        df2 <- NULL
        for (f in list.files("experiments/results/comparison", full.names = TRUE)) {
            res <- readRDS(f)
            tryCatch(df1 <- rbind(df1, res$df1), error = function(e) cat("Could not join df1:", f, "\n"))
            tryCatch(df2 <- rbind(df2, res$df2), error = function(e) cat("Could not join df2:", f, "\n"))
            res <- NULL
        }
        plot_relevance(df2, pdf = TRUE, name = "image")
        plot_relevance2(df2, pdf = TRUE, name = "image")
        # plot_results(df1)
        print_results(df1)
    }
}