## --------------------------------------------------
## Retrieve the UCI "adult" data
## --------------------------------------------------
##
## Usage:
##
##        Rscript --vanilla retrieve_cencus.R
##
## --------------------------------------------------

# This is only run when called from Rscript
if (sys.nframe() == 0L) {
    destdir <- "experiments/data"

    ## --------------------------------------------------
    ## Define Paths
    ## --------------------------------------------------
    data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
    csv_path <- file.path(destdir, "census.csv")
    data_path <- file.path(destdir, "census.rds")

    ## --------------------------------------------------
    ## Retrieve the data
    ## --------------------------------------------------
    download.file(data_url, destfile=csv_path)
    data <- read.csv(csv_path, header = FALSE, strip.white = TRUE)
    colnames(data) <- c("age", "workclass", "fnlwgt", "education",
                        "education_num", "marital_status", "occupation",
                        "relationship", "race", "sex", "capital_gain",
                        "capital_loss", "hours_per_week", "native_country",
                        "income")
    data <- data[data$workclass != "?" & data$occupation != "?" & data$native_country != "?",]
    data <- droplevels(data)
    saveRDS(data, data_path, compress = "xz")
}
