
suppressPackageStartupMessages({
    library(devtools)
    devtools::load_all()
    options(dplyr.summarise.inform = FALSE)
    library(dplyr)
})

RESULTS_DIR <- "experiments/results"

# Use the correct number of cores if running on a slurm-cluster
cores_from_slurm <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
if (is.finite(cores_from_slurm) && cores_from_slurm > 1) {
    options(mc.cores = cores_from_slurm)
    try(setMKLthreads(cores_from_slurm), silent = TRUE)
}

theme_paper <- function() {
    library(ggplot2)
    theme(
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 12),
        panel.spacing.x = unit(0.5, "cm"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 11)
    )
}

get_results_dir <- function(path = RESULTS_DIR) {
    dir.create(file.path(path), showWarnings = FALSE, recursive = TRUE)
    path
}

plot_pdf <- function(plot_obj, name, width, height, path = RESULTS_DIR, verbose = TRUE) {
    path <- get_results_dir(RESULTS_DIR)
    path <- file.path(path, paste0(name, ".pdf"))
    cairo_pdf(path, width * 9, height * 9)
    plot(plot_obj)
    dev.off()
    if (verbose) {
        cat("Plotted to", path, "\n")
    }
}

save_results <- function(data, name, index = -1, tag = NULL, path = RESULTS_DIR) {
    if (index >= 0) {
        path <- get_results_dir(file.path(RESULTS_DIR, name))
        if (!is.null(tag)) {
            path <- file.path(path, sprintf("%s_%s_%02d.rds", name, tag, index))
        } else {
            path <- file.path(path, sprintf("%s_%03d.rds", name, index))
        }
    } else {
        path <- get_results_dir(path)
        if (!is.null(tag)) {
            path <- file.path(path, sprintf("%s_%s.rds", name))
        } else {
            path <- file.path(path, paste0(name, ".rds"))
        }
    }
    saveRDS(data, path, compress = "xz")
}

load_results <- function(name, path = RESULTS_DIR) {
    file <- file.path(RESULTS_DIR, paste0(name, ".rds"))
    dir <- file.path(RESULTS_DIR, name)
    if (file.exists(file)) {
        readRDS(file)
    } else if (file.exists(dir)) {
        do.call(rbind, lapply(list.files(dir, full.names = TRUE), readRDS))
    } else {
        stop("Could not find results")
    }
}

inverse_interpolated <- function(x, y, target) {
    if (target < min(y)) {
        x[which.min(y)]
    } else if (target > max(y)) {
        x[which.max(y)]
    } else {
        diff <- abs(y - target)
        closest <- which_min_n(diff, 2)
        weighted.mean(x[closest], diff[rev(closest)])
    }
}