## -----------------------------------------------------------------------------
## Collect the results from the experiment that has been run in parallell (e.g. scalability)
## -----------------------------------------------------------------------------
## Example usage:
##
##        Rscript --vanilla collect_results.R datapath outputfile
##
## -----------------------------------------------------------------------------

library(plyr)

## Read all rds-files in the given directory,
## and rbind the contents.
collect_results <- function(respath, simple=FALSE) {
    files <- list.files(path = respath, pattern = "*.rds", full.names = TRUE)
    data <- lapply(files, readRDS)
    if (is.data.frame(data[[1]])) {
        do.call(rbind.fill, data)
    } else if (is.list(data[[1]]) && simple) {
        do.call(c, data)
    } else if (is.list(data[[1]])) {
        combined <- list()
        for (n in names(data[[1]])) {
            if (is.atomic(data[[1]][[n]])) {
                comb <- do.call(c, lapply(data, function(d) d[[n]]))
                if (abs(max(comb) - min(comb)) < 1e-12)
                    combined[[n]] <- comb[[1]]
                else
                    combined[[n]] <- comb
            } else if (is.vector(data[[1]][[n]]))
                combined[[n]] <- do.call(c, lapply(data, function(d) d[[n]]))
            else if (is.data.frame(data[[1]][[n]]))
                combined[[n]] <- do.call(rbind.fill, lapply(data, function(d) d[[n]]))
            else
                combined[[n]] <- do.call(rbind, lapply(data, function(d) d[[n]]))
        }
        combined
    }
    else {
        stop("Unknown data to merge")
    }
}

## --------------------------------------------------------------------------------
## Get command-line arguments
## --------------------------------------------------------------------------------

args       <- commandArgs(trailingOnly = TRUE)
respath    <- args[1]                            ## directory with results
outputfile <- args[2]                            ## full path to file where to store the results
simple <- length(args) == 3

## Read all the rds-files in the directory specified by the input path,
## collect the results and output an rds-file.
out <- collect_results(respath, simple)

saveRDS(out, file = outputfile, compress = "xz")

## --------------------------------------------------------------------------------
