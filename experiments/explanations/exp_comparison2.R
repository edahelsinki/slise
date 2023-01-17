## --------------------------------------------------
## Compare the explanations from different model-agnostic post-hoc explainers
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/exp_comparison2.R index iter [python]
##
## Parameters:
##
##        index   : Specify the job index (min=1, max=multiple of 3*6)
##        iter    : Optional number of iterations
##        python  : Optional python executable
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        Run the script without an index to produce plots.
##
## Running Manually:
##        Rscript --vanilla experiments/explanations/exp_comparison2.R 1:18 10
##        Rscript --vanilla experiments/explanations/exp_comparison2.R
## --------------------------------------------------

suppressMessages(source("experiments/explanations/data.R"))
suppressMessages(source("experiments/explanations/methods.R"))

suppressMessages(suppressWarnings(library(e1071)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(dplyr)))


evaluate_expl <- function(data, index, treshold = 0.1, coverage_k = 100, ...) {
    gc_rpy(TRUE)
    dist <- apply(data$X, 1, data$distance_fn, data$X[index, ])
    knn <- order(dist)[2:(coverage_k + 1)]
    inn <- knn[[1]]
    time <- rep(0, 2)
    # Explanation
    time[1] <- proc.time()[3]
    expl <- data$expl_fn(data$X, data$Y, index)
    time[1] <- proc.time()[3] - time[1]
    # Repeat explanation
    time[2] <- proc.time()[3]
    expl2 <- data$expl_fn(data$X, data$Y, index)
    time[2] <- proc.time()[3] - time[2]
    # Approximations
    approx_Y <- expl$approx_fn(data$X)
    approx_Y2 <- expl2$approx_fn(data$X)

    # Fidelity (can the explanation be used for prediction)
    fidelity_a <- c(
        abs(approx_Y[index] - data$Y[index]),
        abs(approx_Y2[index] - data$Y[index])
    )
    fidelity_b <- c(
        mean(abs(expl$approx_fn(expl$neighbourhood$X) - expl$neighbourhood$Y), na.rm = TRUE),
        mean(abs(expl2$approx_fn(expl2$neighbourhood$X) - expl2$neighbourhood$Y), na.rm = TRUE)
    )
    # Coverage (how general is the explanation)
    coverage_a <- c(
        mean(abs(approx_Y - data$Y) < treshold, na.rm = TRUE),
        mean(abs(approx_Y2 - data$Y) < treshold, na.rm = TRUE)
    )
    coverage_b <- c(
        mean(abs(approx_Y[knn] - data$Y[knn]) < treshold, na.rm = TRUE),
        mean(abs(approx_Y2[knn] - data$Y[knn]) < treshold, na.rm = TRUE)
    )
    medianerr <- c(
        median(abs(approx_Y - data$Y), na.rm = TRUE),
        median(abs(approx_Y2 - data$Y), na.rm = TRUE)
    )
    # Consistency (does running the explanation multiple times give you the same result)
    if (is.null(expl$coefficients)) {
        consistency_a <- c(
            mean(abs(expl2$impact - expl$impact)) / mean(abs(expl$impact)),
            mean(abs(expl2$impact - expl$impact)) / mean(abs(expl2$impact))
        )
    } else {
        consistency_a <- c(
            mean(abs(expl2$coefficients - expl$coefficients)) / (mean(abs(expl$coefficients)) + 1e-8),
            mean(abs(expl2$coefficients - expl$coefficients)) / (mean(abs(expl2$coefficients)) + 1e-8)
        )
    }
    consistency_b <- c(
        mean(abs(approx_Y - approx_Y2)),
        mean(abs(approx_Y - approx_Y2))
    )
    # Stability (how well does the NN explanation fit)
    stability_a <- c(
        mean(abs(approx_Y[knn] - data$Y[knn])),
        mean(abs(approx_Y2[knn] - data$Y[knn]))
    )
    stability_b <- c(
        abs(approx_Y[inn] - data$Y[inn]),
        abs(approx_Y2[inn] - data$Y[inn])
    )

    list(
        df1 = data.frame(
            method = factor(expl$name),
            data = factor(data$name),
            fidelity_a = fidelity_a,
            fidelity_b = fidelity_b,
            coverage_a = coverage_a,
            coverage_b = coverage_b,
            medianerr = medianerr,
            intercept = expl$intercept,
            stability_a = stability_a,
            stability_b = stability_b,
            consistency_a = consistency_a,
            consistency_b = consistency_b,
            y = data$Y[index],
            index = index,
            class = data$class,
            time = time,
            ...
        ),
        df2 = rbind(
            # Relevance (how quickly can the explanation destroy the prediction)
            evaluate_relevance(data, index, expl, ...),
            evaluate_relevance(data, index, expl2, ...)
        )
    )
}

evaluate_relevance <- function(data, index, expl, ...) {
    corruption <- seq(0, ncol(data$X))
    inv <- data$destroy_fn(data$X[index, ], expl$coefficients, expl$impact, data$pred_fn)
    inv_max <- do.call(rbind, lapply(corruption[-1], function(i) {
        x <- data$X[index, ]
        x[inv$omax[1:i]] <- inv$xmax[inv$omax[1:i]]
        x
    }))
    inv_min <- do.call(rbind, lapply(corruption[-1], function(i) {
        x <- data$X[index, ]
        x[inv$omin[1:i]] <- inv$xmin[inv$omin[1:i]]
        x
    }))
    inv2_max <- do.call(rbind, lapply(corruption[-1], function(i) {
        x <- data$X[index, ]
        x[inv$omax[i]] <- inv$xmax[inv$omax[i]]
        x
    }))
    inv2_min <- do.call(rbind, lapply(corruption[-1], function(i) {
        x <- data$X[index, ]
        x[inv$omin[i]] <- inv$xmin[inv$omin[i]]
        x
    }))
    relevance_min <- c(0, data$Y[index] - data$pred_fn(inv_min))
    relevance_max <- c(0, data$pred_fn(inv_max) - data$Y[index])
    relevance2_min <- c(0, data$Y[index] - data$pred_fn(inv2_min))
    relevance2_max <- c(0, data$pred_fn(inv2_max) - data$Y[index])
    relevance <- relevance_max + relevance_min
    relevance2 <- relevance2_max + relevance2_min

    data.frame(
        method = factor(expl$name),
        data = factor(data$name),
        variable = corruption,
        corruption = corruption / ncol(data$X),
        relevance_max = relevance_max,
        relevance_min = relevance_min,
        relevance = relevance,
        relevance2_max = relevance2_max,
        relevance2_min = relevance2_min,
        relevance2 = relevance2,
        index = index,
        ...
    )
}

make_tabular_inverse <- function(X) {
    d <- ncol(X)
    sdX <- apply(X, 2, sd)
    function(x, coef, impact, pred_fn) {
        if (is.null(coef)) {
            mod <- diag(sdX)
            xpos <- sweep(mod, 2, c(x), `+`)
            xneg <- sweep(-mod, 2, c(x), `+`)
            pos <- c(pred_fn(xpos)) > c(pred_fn(xneg))
            # Negative impacts are destroyed
            xmax <- ifelse(impact >= -1e-8, x, x + ifelse(pos, sdX, -sdX))
            omax <- order(impact, decreasing = FALSE)
            # Positive impacts are destroyed
            xmin <- ifelse(impact <= 1e-8, x, x + ifelse(pos, -sdX, sdX))
            omin <- order(impact, decreasing = TRUE)
        } else {
            xpos <- x + sdX
            xneg <- x - sdX
            pos <- coef * xpos > coef * xneg
            # Negative impacts are inversed
            xmax <- ifelse(abs(coef) < 1e-8, x, ifelse(pos, xpos, xneg))
            omax <- order(xmax * coef - x * coef, decreasing = TRUE)
            # Positive impacts are inversed
            xmin <- ifelse(abs(coef) < 1e-8, x, ifelse(pos, xneg, xpos))
            omin <- order(xmin * coef - x * coef, decreasing = FALSE)
        }
        list("xmax" = xmax, "omax" = omax, "xmin" = xmin, "omin" = omin)
    }
}

get_config <- function(index) {
    index <- index - 1
    set.seed(42 + index)
    reticulate::py_set_seed(as.integer(42 + index))
    methods <- list(
        "slise" = slise_tabular,
        "shap" = shap_tabular,
        "lime_nd" = function(...) lime_tabular(..., discretise = FALSE),
        "lime" = function(...) lime_tabular(..., discretise = TRUE),
        "global" = global_tabular,
        "random" = rndexpl_tabular
    )
    datasets <- list(
        "cars" = function() data_mtcars(scale = TRUE),
        "physics" = function() data_jets(pred_fn = TRUE, scale = TRUE),
        "imdb" = function() data_imdb(pred_fn = TRUE)
    )
    df <- expand.grid(
        method = names(methods),
        dataset = names(datasets)
    )
    index <- index %% nrow(df) + 1
    data <- datasets[[df$dataset[index]]]()
    data$method <- df$method[index]
    data$expl_fn <- methods[[data$method]]
    data$destroy_fn <- make_tabular_inverse(data$X)
    data$distance_fn <- euclidean_distance
    data
}

get_index <- function(params, index = -1) {
    # max_n should match the n in exp_param_select.R
    max_n <- if (params$name %in% c("imdb", "jet images")) 5000 else 10000
    data <- params
    if (max_n < length(data$Y)) {
        if (data$class) {
            mask <- c()
            lvls <- if (is.factor(data$R)) levels(data$R) else 1:0
            for (l in lvls) mask <- c(mask, sample(which(data$R == l), max_n / length(lvls)))
            mask <- sample(mask)
        } else {
            mask <- sample.int(length(Y), max_n)
        }
        if (index > 0) {
            mask <- c(index, mask[mask != index])
            data$index <- 1
        }
        data$R <- params$R[mask]
        data$Y <- params$Y[mask]
        data$X <- params$X[mask, ]
    } else if (index > 0) {
        data$index <- index
    }
    if (is.null(data$index)) {
        data$index <- sample.int(nrow(data$X), 1)
    }
    if (data$class) {
        if (data$Y[data$index] < 0.5) {
            data$Y <- 1 - data$Y
            data$pred_fn <- function(...) params$pred_fn(...)[, 1]
        } else {
            data$pred_fn <- function(...) params$pred_fn(...)[, 2]
        }
    }
    if (data$name == "imdb") {
        columns <- data$X[data$index, ] != 0
        data$X <- data$X[, columns]
        expand <- diag(ncol(params$X))[columns, ]
        pred_fn <- data$pred_fn
        data$pred_fn <- function(X, ...) pred_fn(X %*% expand, ...)
        data$destroy_fn <- make_tabular_inverse(data$X)
    }
    data$expl_fn <- function(...) {
        params$expl_fn(
            ...,
            predict_fn = data$pred_fn,
            class = data$class,
            epsilon = data$epsilon,
            lambda1 = data$lambda1,
            lambda2 = data$lambda2,
            distance_fn = data$distance_fn
        )
    }
    data
}


plot_relevance <- function(df2, treshold = 0.8, pdf = FALSE, name = "tabular", path = NULL) {
    if (missing(path)) path <- file.path("experiments", "results", sprintf("comparison_relevance_%s.pdf", name))
    width <- 0.2 + 0.28 * length(levels(df2$data))
    if (pdf) cairo_pdf(path, width = width * 9, height = 0.35 * 9)
    levels(df2$data) <- stringr::str_replace_all(levels(df2$data), c("mtcars" = "MTCars", "jets" = "JETS", "imdb" = "IMDB", "emnist" = "EMNIST", "jet images" = "JET Images"))
    if (name == "image") {
        df2 <- df2 %>%
            group_by(method, data, corruption) %>%
            summarise(relevance = mean(relevance))
    }
    df2 <- df2 %>%
        filter(corruption < treshold) %>%
        group_by(method, data) %>%
        mutate(weight = (corruption == 0) * length(corruption) + 1)
    plot(ggplot(df2) +
        facet_wrap(vars(data), ncol = 3, scales = "free") +
        geom_smooth(
            aes(corruption, relevance, group = method, linetype = method, color = method, weight = weight),
            method = "loess",
            span = 0.6,
            se = FALSE
        ) +
        xlab("Fraction of corrupted variables") +
        ylab("Prediction change") +
        theme_paper(legend.key.width = unit(3, "line")))
    if (pdf) dev.off()
    if (pdf) cat("Plot saved to:", path, "\n")
}

plot_relevance2 <- function(df2, pdf = FALSE, name = "tabular", path = NULL) {
    if (missing(path)) path <- file.path("experiments", "results", sprintf("comparison_relevance2_%s.pdf", name))
    width <- 0.16 + 0.28 * length(levels(df2$data))
    if (pdf) cairo_pdf(path, width = width * 9, height = 0.35 * 9)
    levels(df2$data) <- stringr::str_replace_all(levels(df2$data), c("mtcars" = "MTCars", "jets" = "JETS", "imdb" = "IMDB", "emnist" = "EMNIST", "jet images" = "JET Images"))
    df2 <- df2 %>% filter(corruption > 0)
    if (name == "image") {
        df2 <- df2 %>%
            group_by(method, data, variable) %>%
            summarise(relevance2 = mean(relevance2))
    }
    plot(ggplot(df2) +
        facet_wrap(vars(data), ncol = 3, scales = "free") +
        geom_smooth(
            aes(variable, relevance2, group = method, linetype = method, color = method),
            method = "loess",
            se = FALSE,
            span = 0.6
        ) +
        xlab("Corrupted variable") +
        ylab("Prediction change") +
        theme_paper(legend.key.width = unit(2, "line")))
    if (pdf) dev.off()
    if (pdf) cat("Plot saved to:", path, "\n")
}

plot_results <- function(df1) {
    plot(
        ggplot(df1 %>% pivot_longer(c(fidelity_a, coverage_a, consistency_a, stability_b))) +
            facet_wrap(vars(name, data), ncol = 3, scales = "free") +
            geom_boxplot(aes(method, value, color = method)) +
            theme_paper()
    )
}

print_results <- function(df1) {
    minim <- function(x) mean(x) + sd(x) / 2 + 1e-6
    maxim <- function(x) mean(x) - sd(x) / 2 - 1e-6
    best <- df1 %>%
        group_by(data, method) %>%
        summarise(Fidelity = minim(fidelity_a), Coverage = maxim(coverage_a), Stability = minim(stability_b)) %>%
        group_by(data) %>%
        summarise(Fidelity = min(Fidelity), Coverage = max(Coverage), Stability = min(Stability))
    format_cell <- function(x, dat, metric, maximise) {
        mx <- mean(x)
        val <- (best %>% filter(data == dat[1]))[[metric]][1]
        if ((maximise && mx > val) || (!maximise && mx < val)) {
            sprintf("$\\bf{%.3f \\pm %.3f}$", mx, sd(x))
        } else {
            sprintf("$%.3f \\pm %.3f$", mx, sd(x))
        }
    }
    df <- df1 %>%
        group_by(data, method) %>%
        summarise(
            Fidelity = format_cell(fidelity_a, data, "Fidelity", FALSE),
            Coverage = format_cell(coverage_a, data, "Coverage", TRUE),
            Stability = format_cell(stability_b, data, "Stability", FALSE)
        )
    tab <- print(
        xtable::xtable(
            df,
            align = c("l", "l", "l", rep("r", ncol(df) - 2)),
            label = "tab:comp:tab",
            caption = "TODO"
        ),
        sanitize.text.function = identity,
        sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
        include.rownames = FALSE,
        comment = FALSE,
        booktabs = TRUE,
        print.results = FALSE,
        table.placement = "t",
        caption.placement = "top"
    )
    tab <- stringr::str_replace(tab, "centering", "centering\n% \\\\rowcolors{2}{white}{gray!25}")
    replacements <- c(
        "mtcars" = "\\\\mtcars",
        "jets" = "\\\\jets",
        "jet images" = "\\\\jetimages",
        "imdb" = "\\\\imdb",
        "SLISE" = "\\\\slise",
        "LIME" = "\\\\lime",
        "SHAP" = "\\\\shap",
        "emnist" = "\\\\emnist",
        "data" = "Data",
        "method" = "Method",
        "gray" = "grey"
    )
    cat(stringr::str_replace_all(tab, replacements))
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
        dir.create("experiments/results/comparison2", showWarnings = FALSE, recursive = TRUE)
        for (job_index in eval(parse(text = args[1]))) {
            suppressMessages(params <- get_config(job_index))
            file <- sprintf("experiments/results/comparison2/%03d_%s_%s.rds", job_index, params$name, params$method)
            if (!file.exists(file)) {
                df1 <- NULL
                df2 <- NULL
                time <- proc.time()[3]
                time2 <- proc.time()[3]
                cat(sprintf("[comparison2] Init %3d: %s %s\n", job_index, params$name, params$method))
                for (i in 1:iterations) {
                    if (proc.time()[3] - time2 > 120) {
                        saveRDS(list(df1 = df1, df2 = df2), file, compress = "xz")
                        time2 <- proc.time()[3]
                    }
                    data <- get_index(params)
                    cat(sprintf("[comparison2] Iter %3d: %3d\n", job_index, i))
                    res <- evaluate_expl(data, data$index, iter = i, job = job_index)
                    df1 <- rbind(df1, res$df1)
                    df2 <- rbind(df2, res$df2)
                }
                saveRDS(list(df1 = df1, df2 = df2), file, compress = "xz")
                time <- proc.time()[3] - time
                cat(sprintf("[comparison2] Done %3d: %.1f seconds\n", job_index, time))
            } else {
                cat("[comparison2] Exists:", job_index, "\n")
            }
        }
    } else {
        df1 <- NULL
        df2 <- NULL
        for (f in list.files("experiments/results/comparison2", full.names = TRUE)) {
            res <- readRDS(f)
            tryCatch(df1 <- rbind(df1, res$df1), error = function(e) cat("Could not join df1:", f, "\n"))
            tryCatch(df2 <- rbind(df2, res$df2), error = function(e) cat("Could not join df2:", f, "\n"))
            res <- NULL
        }
        plot_relevance(df2, pdf = TRUE)
        plot_relevance2(df2, pdf = TRUE)
        # plot_results(df1)
        print_results(df1)
    }
}