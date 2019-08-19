## --------------------------------------------------
## Peform experiment for Robust Regression Scalability
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla experiments/exp_scalability.R destdir cset
##
## Parameters:
##
##        destdir : Destination directory for the results
##        cset    : Specify a certain set to run (integer 1 - 22).
##
## Notes:
##        This is designed to be run in parallel on a cluster
##        since some of the configurations WILL pass the time
##        limit without honoring interrupts. If these are run
##        manually (without a cluster) then you might need to
##        cancel some executions (fastLTS et.c.) after a couple
##        of hours (all valid results are saved).
##
## Running Manually:
##        Rscript --vanilla experiments/exp_scalability.R results/tmp/scalability 1
##        ...
##        Rscript --vanilla experiments/exp_scalability.R results/tmp/scalability 22
##        Rscript --vanilla experiments/collect_results.R results/tmp/scalability results/scalability.rds
## --------------------------------------------------

## --------------------------------------------------
## Libraries
## --------------------------------------------------
suppressMessages(source("experiments/utils.R"))
suppressMessages(library(R.utils)) # For timeout

## -----------------------------------------------------------------------------
## Generate list of all configurations,
## then choose a subset based on the input arguments (variable 'cset')
## The num_csets must be equal to the number of parallel jobs (which is 22)
## -----------------------------------------------------------------------------
scalability_configs <- function(cset=1, num_csets=1, methods = METHODS) {
    lambdas <- c(0, 0.01, 0.1, 0.5)
    configs <- expand.grid(method=methods, lambda=lambdas, stringsAsFactors=FALSE)
    configs <- configs[!(configs$lambda > 0 & configs$method %in% c("fastlts", "ols", "mm")), ]
    # One method variation per job (to avoid stalling and crashing causing missing values)
    if (num_csets != nrow(configs))
        stop(sprintf("Number of csets not equal to the number of method variations (%d)", nrow(configs)))
    methods <- configs$method[cset]
    lambdas <- configs$lambda[cset]

    ns          <-  c(100, 500, 1000, 5000, 10000, 50000, 100000) ## as.integer(10^seq(2, 5, 0.5))
    ds          <-  c(10, 50, 100, 500, 1000)                     ## as.integer(10^seq(1, 3, 0.5))
    epsilons    <-  c(0.1)

    configs     <- expand.grid(method=methods, epsilon = epsilons, lambda=lambdas, d = ds, n = ns, stringsAsFactors=FALSE)
    configs     <- configs[(configs$d * 4 < configs$n), ]
    configs     <- configs[configs$d < 200 | configs$n < 20000, ]
    # Order by estimated time
    configs <- configs[order(configs$d ^ 2 * 2 + configs$n), ]
    configs["index"] <- (cset - 1) * nrow(configs) + 1:nrow(configs)
    configs
}

scalability_data <- function(configs, index, interation = 1) {
    set.seed(configs$n[[index]] * 1e2 + configs$d[[index]] + interation)
    with(configs[index, ], data_create(n, d, floor(d * 0.3), epsilon))
}


## -----------------------------------------------------------------------------
scalability_calculate <- function(configs, iterations=3, filename=NULL) {
    num       <- nrow(configs)
    times     <- as.numeric(rep(NA, num * iterations))
    losses    <- as.numeric(rep(NA, num * iterations))
    residuals <- as.numeric(rep(NA, num * iterations))
    subset    <- as.numeric(rep(NA, num * iterations))
    sparsity  <- as.numeric(rep(NA, num * iterations))
    iter      <- rep(1:iterations, num)
    configs   <- configs[rep(1:num, each=iterations), ]

    export <- function() {
        configs["time"]      <<- times
        configs["loss"]      <<- losses
        configs["residuals"] <<- residuals
        configs["subset"]    <<- subset
        configs["sparsity"]  <<- sparsity
        configs["iteration"] <<- iter
        if (!is.null(filename))
            saveRDS(configs, filename, compress = "xz")
    }

    for (i in 1:num) {
        i <- (i - 1) * iterations
        gc()
        tryCatch(
            expr = {
                for (j in 1:iterations) {
                    data <- scalability_data(configs, i + 1, j)
                    times[i + j] <- system.time(withTimeout(
                        slise <- regression(method=configs$method[[i + 1]], X=data$X, Y=data$Y, epsilon=configs$epsilon[[i + 1]], lambda=configs$lambda[[i + 1]]),
                        timeout=600, cpu=1200, elapsed=600))[[3]]
                    losses[[i + j]] <- slise$value
                    residuals[[i + j]] <- sum(abs(data$clean - data$X %*% slise$coefficients[-1] - slise$coefficients[[1]]))
                    subset[[i + j]] <- mean(slise$subset)
                    sparsity[[i + j]] <- sparsity(slise$alpha)
                }
            },
            TimeoutException = function(ex) {
                cat("\n", "Timeout in:", configs$n[[i + 1]], "x", configs$d[[i + 1]], configs$method[[i + 1]], "\n")
            },
            error = function(e) {
                cat("\n", "Error in:", configs$n[[i + 1]], "x", configs$d[[i + 1]], configs$method[[i + 1]], "\n")
                print(e)
            }
        )
        export()
        print(sprintf("%12s: %dx%d", configs$method[[i + 1]], configs$n[[i + 1]], configs$d[[i + 1]]))
    }

    cat("\nDone!\n")
    configs
}
## -----------------------------------------------------------------------------



## --------------------------------------------------
## If run from Rscript
## --------------------------------------------------
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) < 1) {
        destdir <- "results/scalability"
    } else {
        destdir <- args[1]
    }
    dir.create(file.path(destdir), showWarnings = FALSE, recursive = TRUE)

    if (length(args) < 2) {
        cset <- 1
        num_csets <- 1
    } else {
        cset <- as.numeric(args[2])
    }
    if (length(args) < 3) {
        num_csets <- 15
    } else {
        num_csets <- as.numeric(args[3])
    }
    if (num_csets > 1)
        filename <- file.path(destdir, sprintf("scalability_%03d.rds", cset))
    else
        filename <- file.path(destdir, "scalability.rds")

    configs <- scalability_configs(cset, num_csets)
    dir.create(file.path(destdir), showWarnings = FALSE, recursive = TRUE)
    cat("Scalability Benchmark:", cset, "/", num_csets, "\n")
    p <- scalability_calculate(configs, iterations = 5, filename=filename)
}
