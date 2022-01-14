## --------------------------------------------------
## Compare the explanations from different model-agnostic post-hoc explainers
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/exp_comparison.R index [python]
##
## Parameters:
##
##        index   : Specify the job index (1-90)
##        python  : Optional python executable
##
## Notes:
##        This is designed to be run in parallel on a cluster.
##        The number of jobs should be a multiple of 90.
##        Run the script without an index to produce plots.
##
## Running Manually:
##        Rscript --vanilla experiments/explanations/exp_comparison.R 1:90
##        Rscript --vanilla experiments/explanations/exp_comparison.R
## --------------------------------------------------

library(reticulate)
library(keras)
suppressMessages(suppressWarnings(library(dplyr)))

suppressMessages(source("experiments/explanations/data.R"))
suppressMessages(source("experiments/explanations/methods.R"))


evaluate_expl <- function(expl_fn, predict_fn, data, index, destroy_fn, treshold = 0.1, relevance_k = 0.1, coverage_k = 100, coherence_k = 20, stability_k = 20, stability_noise = 0.2) {
    dist <- apply(data$X, 1, data$distance_fn, data$X[index, ])
    knn <- order(dist)[2:(coverage_k + 1)]
    expl <- expl_fn(data$X, data$Y, index, predict_fn)
    approx_Y <- expl$approx_fn(data$X)
    # Fidelity (can the explanation be used for prediction)
    fidelity_a <- abs(approx_Y[index] - data$Y[index])
    fidelity_b <- mean(abs(expl$approx_fn(expl$neighbourhood$X) - expl$neighbourhood$Y), na.rm = TRUE)
    # Coverage (how general is the explanation)
    coverage_a <- mean(abs(approx_Y - data$Y) < treshold, na.rm = TRUE)
    coverage_b <- mean(abs(approx_Y[knn] - data$Y[knn]) < treshold, na.rm = TRUE)
    medianerr <- median(abs(approx_Y - data$Y), na.rm = TRUE)
    # Relevance (how quickly can the explanation destroy the prediction)
    if (relevance_k < 1) {
        relevance_k <- relevance_k * ncol(data$X)
    }
    corruption <- seq(1, relevance_k, length.out = 20)
    important <- order(expl$impact, decreasing = TRUE)
    inverse <- destroy_fn(data$X[index, ])
    impinv <- do.call(rbind, lapply(corruption, function(i) {
        x <- data$X[index, ]
        x[important[1:i]] <- inverse[important[1:i]]
        x
    }))
    relevance <- c(1, predict_fn(impinv) / data$Y[index])
    gc_rpy(TRUE)
    # Coherence (do the knn have the same explanation)
    x_orig <- data$X[index, , drop = FALSE]
    y_orig <- data$Y[index]
    x_norm_a <- NULL
    hm_norm_a <- NULL
    fx_a <- NULL
    fix_a <- NULL
    fxi_a <- NULL
    fixi_a <- NULL
    for (i in order(dist)[2:(coherence_k + 1)]) {
        if (i != index) {
            expl2 <- expl_fn(data$X, data$Y, i, predict_fn)
            x_new <- data$X[i, , drop = FALSE]
            # Adding the (implicit) intercept not present in the saliency maps
            x_norm_a <- c(x_norm_a, euclidean_distance(c(1, x_new), c(1, x_orig)))
            hm_norm_a <- c(hm_norm_a, euclidean_distance(
                c(expl$alpha[1], expl$heatmap),
                c(expl2$alpha[1], expl2$heatmap)
            ))
            fx_a <- c(fx_a, c(expl$approx_fn(x_orig)))
            fix_a <- c(fix_a, c(expl2$approx_fn(x_orig)))
            fxi_a <- c(fxi_a, c(expl$approx_fn(x_new)))
            fixi_a <- c(fixi_a, c(expl2$approx_fn(x_new)))
            expl2 <- NULL
            gc_rpy(TRUE)
        }
    }
    stability_a <- max(hm_norm_a / x_norm_a)
    coherence_a1 <- max(sqrt((fx_a - fxi_a)^2 + (fix_a - fixi_a)^2) / x_norm_a)
    coherence_a2 <- max(abs(fix_a - fixi_a - fx_a + fxi_a) / x_norm_a^2)
    # Stability (does stability_noise alter the explanation)
    x_norm_b <- NULL
    hm_norm_b <- NULL
    fx_b <- NULL
    fix_b <- NULL
    fxi_b <- NULL
    fixi_b <- NULL
    minX <- apply(data$X, 2, min)
    maxX <- apply(data$X, 2, max)
    for (i in rep(index, stability_k)) {
        mask <- sample.int(ncol(data$X), ncol(data$X) * stability_noise)
        data$X[i, mask] <- runif(length(mask), minX[mask], maxX[mask])
        data$Y[i] <- predict_fn(data$X[i, , drop = FALSE])
        expl2 <- expl_fn(data$X, data$Y, i, predict_fn)
        x_new <- data$X[i, , drop = FALSE]
        x_norm_b <- c(x_norm_b, euclidean_distance(c(1, x_new), c(1, x_orig)))
        hm_norm_b <- c(hm_norm_b, euclidean_distance(
            c(expl$alpha[1], expl$heatmap),
            c(expl2$alpha[1], expl2$heatmap)
        ))
        fx_b <- c(fx_b, c(expl$approx_fn(x_orig)))
        fix_b <- c(fix_b, c(expl2$approx_fn(x_orig)))
        fxi_b <- c(fxi_b, c(expl$approx_fn(x_new)))
        fixi_b <- c(fixi_b, c(expl2$approx_fn(x_new)))
        expl2 <- NULL
        gc_rpy(TRUE)
    }
    stability_b <- max(hm_norm_b / x_norm_b)
    coherence_b1 <- max(sqrt((fx_b - fxi_b)^2 + (fix_b - fixi_b)^2) / x_norm_b)
    coherence_b2 <- max(abs(fix_b - fixi_b - fx_b + fxi_b) / x_norm_b^2)

    data$X[index, ] <- x_orig
    data$Y[index] <- y_orig

    list(
        df1 = data.frame(
            method = expl$name,
            data = data$name,
            fidelity_a = fidelity_a,
            fidelity_b = fidelity_b,
            coverage_a = coverage_a,
            coverage_b = coverage_b,
            medianerr = medianerr,
            stability_a = stability_a,
            stability_b = stability_b,
            coherence_a1 = coherence_a1,
            coherence_b1 = coherence_b1,
            coherence_a2 = coherence_a2,
            coherence_b2 = coherence_b2,
            relevance = median(relevance),
            heatmap_std = sd(expl$heatmap),
            heatmap_max = max(abs(expl$heatmap)),
            heatmap_mean = mean(abs(expl$heatmap)),
            intercept = expl$alpha[1],
            alpha_tot = sum(abs(expl$alpha))
        ),
        df2 = data.frame(
            method = expl$name,
            data = data$name,
            corruption = c(0, corruption / ncol(data$X)),
            relevance = relevance
        ),
        df3 = data.frame(
            x_norm_a = x_norm_a,
            hm_norm_a = hm_norm_a,
            fx_a = fx_a,
            fix_a = fix_a,
            fxi_a = fxi_a,
            fixi_a = fixi_a,
            x_norm_b = x_norm_b,
            hm_norm_b = hm_norm_b,
            fx_b = fx_b,
            fix_b = fix_b,
            fxi_b = fxi_b,
            fixi_b = fixi_b
        )
    )
}

get_config <- function(index) {
    index <- index - 1
    method <- index %% 10
    index <- floor(index / 10)
    dataset <- index %% 10
    set.seed(42 + index)
    reticulate::py_set_seed(as.integer(42 + index))
    if (dataset < 10) {
        data <- data_emnist(dataset)
        data$distance_fn <- cosine_distance
        model <- keras::load_model_hdf5("experiments/data/emnist_model.hdf5")
        pred_fn <- function(x) {
            dim(x) <- c(length(x) / 784, 784)
            out <- predict(model, x)
            out[, dataset + 1, drop = FALSE]
        }
        index <- which(data$Y > 0.5 & data$R == dataset)[[1]]
        expl_fn <- if (method == 0) {
            slise_emnist
        } else if (method == 1) {
            distslise_emnist
        } else if (method == 2) {
            limeslise_emnist
        } else if (method == 3) {
            function(...) lime_emnist(..., segmentation = "original")
        } else if (method == 4) {
            function(...) lime_emnist(..., segmentation = "small")
        } else if (method == 5) {
            function(...) lime_emnist(..., segmentation = "pixel")
        } else if (method == 6) {
            function(...) shap_emnist(..., deletion = "gray", mid = 0.5)
        } else if (method == 7) {
            function(...) shap_emnist(..., deletion = "invert", low = 0, mid = 0.5, high = 1)
        } else if (method == 8) {
            function(...) shap_emnist(..., deletion = "sample")
        } else if (method == 9) {
            rndexpl_emnist
        } else {
            stop("Unknown explanation method")
        }
        destroy_fn <- function(x) 1 - x
    } else {
        stop("Other datasets are not implemented yet")
    }
    list(
        data = data,
        pred_fn = pred_fn,
        index = index,
        expl_fn = expl_fn,
        destroy_fn = destroy_fn
    )
}

gc_rpy <- function(full = FALSE) {
    gc(verbose = FALSE, full = full)
    reticulate::import("gc")$collect(as.integer(ifelse(full, 2, 1)))
}

plot_lineup <- function(dir = "experiments/results") {
    cat("Plotting a lineup of explanations\n")
    contour <- NULL
    heatmap <- NULL
    labels <- NULL
    offset <- 2 * 10
    for (i in offset + 1:9) {
        params <- get_config(i)
        expl <- params$expl_fn(params$data$X, params$data$Y, params$index, params$pred_fn)
        contour <- rbind(contour, params$data$X[params$index, ])
        heatmap <- rbind(heatmap, expl$heatmap)
        labels <- c(labels, stringr::str_replace(expl$name, "Weighted-SLISE", "SLISE (weighted)"))
        cat(" ", expl$name, "\n")
    }
    cairo_pdf(file.path(dir, "comparison_lineup.pdf"), width = 0.8 * 9, height = 0.8 * 9)
    plot(plot_mnist(
        array(t(apply(heatmap, 1, function(x) x / max(abs(x)))), c(length(labels), 28, 28)),
        array(contour, c(length(labels), 28, 28)),
        c("not 2", "2"),
        enhance_colours = FALSE
    ) + facet_wrap(
        vars(Var1),
        nrow = 3,
        labeller = function(x) data.frame(Var1 = labels)
    ) + theme_image(legend.position = "None"))
    dev.off()
}

plot_relevance <- function(df2, max_cor = 0.05, dir = "experiments/results") {
    df <- df2 %>%
        mutate(method = factord(stringr::str_replace(method, "Weighted-SLISE", "SLISE (weighted)"))) %>%
        select(-data) %>%
        # filter(corruption <= max_cor) %>%
        group_by(method, corruption) %>%
        summarise_all(mean)
    cairo_pdf(file.path(dir, "comparison_relevance.pdf"), width = 0.6 * 9, height = 0.35 * 9)
    plot(ggplot(df) +
        geom_smooth(aes(corruption, relevance, group = method, linetype = method, size = method, color = method), se = FALSE) +
        scale_size_manual(values = c(rep(1.5, 3), rep(1, 3), rep(0.7, 3), 1.4)) +
        scale_linetype_manual(values = c(rep(c(1, 4, 2), 3), 9)) +
        scale_color_manual(values = c(rep(SLISE_DARKPURPLE, 3), rep(SLISE_ORANGE, 3), rep("#1b9e77", 3), "black")) +
        xlab("Fraction of Corruption") +
        ylab("Relative Prediction") +
        coord_cartesian(xlim = c(0, max_cor)) +
        theme_paper(legend.key.width = unit(3, "line")))
    dev.off()
}

table_results <- function(res) {
    format <- function(means, sds, best = which.max, decimals = 3) {
        means <- round(means, decimals)
        sds <- round(sds, decimals)
        mask <- best(means)
        mask <- (means <= means[mask] + sds[mask] & means >= means[mask] - sds[mask]) + 1
        fmt <- sprintf("%%.%df", decimals)
        fmt <- paste0(c("$", "$\\bf{")[mask], fmt, " \\pm ", fmt, c("$", "}$")[mask])
        sprintf(fmt, means, sds)
    }
    sanitise_names <- function(x) {
        x <- stringr::str_replace(x, "Distance-SLISE", "SLISE (distance)")
        x <- stringr::str_replace(x, "Euclidean-SLISE", "SLISE (distance)")
        x <- stringr::str_replace(x, "Weighted-SLISE", "SLISE (weighted)")
        x <- stringr::str_replace(x, "LIME", "\\\\lime")
        x <- stringr::str_replace(x, "SHAP", "\\\\shap")
        x <- stringr::str_replace(x, "SLISE", "\\\\slise")
        x <- stringr::str_replace(x, "gray", "grey")
        x
    }
    df <- res %>%
        mutate(
            method = factord(method),
            stability2_a = stability_a / heatmap_max,
            stability2_b = stability_b / heatmap_max
        ) %>%
        filter(stringr::str_starts(data, "emnist")) %>%
        select(-data, -relevance) %>%
        group_by(method) %>%
        summarise_all(list(mean = mean, sd = sd)) %>%
        transmute(
            Method = sanitise_names(method),
            `Fidelity (Item)` = format(fidelity_a_mean, fidelity_a_sd, which.min),
            `Fidelity (Subset)` = format(fidelity_b_mean, fidelity_b_sd, which.min),
            `Coverage (Dataset)` = format(coverage_a_mean, coverage_a_sd, which.max),
            `Coverage (100 NN)` = format(coverage_b_mean, coverage_b_sd, which.max),
            `Median Error` = format(medianerr_mean, medianerr_sd, which.min),
            `Stability (20 NN)` = format(stability_a_mean, stability_a_sd, which.min),
            `Stability (Noise)` = format(stability_b_mean, stability_b_sd, which.min),
            `Coherence (20 NN)` = format(coherence_a2_mean, coherence_a2_sd, which.min),
            `Coherence (Noise)` = format(coherence_b2_mean, coherence_b2_sd, which.min),
            `Stability2 (20 NN)` = format(stability2_a_mean, stability2_a_sd, which.min),
            `Stability2 (Noise)` = format(stability2_b_mean, stability2_b_sd, which.min),
            `Coherence2 (20 NN)` = format(coherence_a1_mean, coherence_a1_sd, which.min),
            `Coherence2 (Noise)` = format(coherence_b1_mean, coherence_b1_sd, which.min),
            `sd(hm)` = format(heatmap_std_mean, heatmap_std_sd, which.max),
            `mean(|hm|)` = format(heatmap_mean_mean, heatmap_mean_sd, which.max),
            `max(|hm|)` = format(heatmap_max_mean, heatmap_max_sd, which.max),
            intercept = format(intercept_mean, intercept_sd, which.min),
            `sum(|alpha|)` = format(alpha_tot_mean, alpha_tot_sd, which.max)
        )
    table <- function(selection, label, caption = "TODO") {
        tab <- print(
            xtable::xtable(
                as.data.frame(df %>% select(c("Method", selection))),
                align = c("l", "l", rep("r", length(selection))),
                label = paste0("tab:comp:", label),
                caption = caption
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
        tab <- stringr::str_replace(tab, "centering", "centering\n\\\\rowcolors{2}{white}{gray!25}")
        paste0("\n", tab)
    }
    cat(table(c("Fidelity (Item)", "Fidelity (Subset)"), "fidelity"))
    cat(table(c("Coverage (Dataset)", "Coverage (100 NN)", "Median Error"), "coverage"))
    cat(table(c("Stability (20 NN)", "Stability (Noise)", "Coherence (20 NN)", "Coherence (Noise)"), "stability"))
    # cat(table(c("Stability2 (20 NN)", "Stability2 (Noise)", "Coherence2 (20 NN)", "Coherence2 (Noise)"), "stability2"))
    # cat(table(c("mean(|hm|)", "sd(hm)", "max(|hm|)", "sum(|alpha|)", "intercept"), "stats"))
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
        dir.create("experiments/results/comparison", showWarnings = FALSE, recursive = TRUE)
        for (index in eval(parse(text = args[1]))) {
            file <- sprintf("experiments/results/comparison/comparison_%03d.rds", index)
            if (!file.exists(file)) {
                time <- Sys.time()
                suppressMessages(params <- get_config(index))
                cat("[comparison] Init:", index, "\n")
                res <- evaluate_expl(params$expl_fn, params$pred_fn, params$data, params$index, params$destroy_fn)
                saveRDS(res, file)
                time <- Sys.time() - time
                cat("[comparison] Done:", index, "in", as.integer(time), "seconds\n")
            } else {
                cat("[comparison] Exists:", index, "\n")
            }
        }
    } else {
        df1 <- NULL
        df2 <- NULL
        for (f in list.files("experiments/results/comparison", full.names = TRUE)) {
            res <- readRDS(f)
            # res$df1$coherence_a1 = max(abs(res$df3$fx_a - res$df3$fxi_a - res$df3$fix_a + res$df3$fixi_a) / res$df3$x_norm_a)
            # res$df1$coherence_b1 = max(abs(res$df3$fx_b - res$df3$fxi_b - res$df3$fix_b + res$df3$fixi_b) / res$df3$x_norm_b)
            tryCatch(df1 <- rbind(df1, res$df1), error = function(e) cat("Could not join df1:", f, "\n"))
            tryCatch(df2 <- rbind(df2, res$df2), error = function(e) cat("Could not join df2:", f, "\n"))
            res <- NULL
        }
        plot_lineup()
        plot_relevance(df2)
        table_results(df1)
    }
}