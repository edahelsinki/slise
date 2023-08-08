# Helper methods for the experiments

suppressMessages(library(devtools))
suppressMessages(devtools::load_all())
options(matprod = "blas")
suppressMessages(suppressWarnings(library(ggplot2)))

# Use the correct number of cores if running on a slurm-cluster
cores_from_slurm <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
if (is.finite(cores_from_slurm) && cores_from_slurm > 1) {
    options(mc.cores = cores_from_slurm)
}

# Hide Tensoflow INFO and WARNING logs (e.g. cuda missing)
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 2)


gc_rpy <- function(full = FALSE) {
    gc(verbose = FALSE, full = full)
    reticulate::import("gc")$collect(as.integer(ifelse(full, 2, 1)))
}


euclidean_distance <- function(x, y) sqrt(sum((x - y)^2))
cosine_distance <- function(x, y) 1 - x %*% y / sqrt((x %*% x) * (y %*% y))

theme_paper <- function(..., legend.title = element_blank()) {
    theme_bw() +
        theme(
            legend.title = legend.title,
            strip.background = element_blank(),
            strip.text = element_text(color = "black", size = 12),
            panel.spacing.x = unit(0.5, "cm"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 11),
            ...
        )
}

theme_image <- function(..., aspect.ratio = 1) {
    theme_bw() +
        theme(
            legend.title = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(color = "black", size = 12),
            panel.spacing.x = unit(0.0, "cm"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            # panel.border = element_rect(colour = "black"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            aspect.ratio = aspect.ratio,
            ...
        )
}

# Factor where the levels are not sorted
factord <- function(x, ...) factor(x, unique(x), ...)