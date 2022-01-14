# Experiments for the Robust Regression paper

## Required Packages

```R
install.packages(c(
    "devtools",
    "roxygen2",
    "Rcpp",
    "RcppArmadillo",
    "lbfgs",
    "dplyr",
    "tidyr",
    "ggplot2",
    "glmnet",
    "robustbase",
    "reticulate",
    "R.utils",
    "ggrepel",
    "lemon",
    "xtable",
    "robustHD",
    "MTE",
    "conquer"
), Ncpus = 4)
## If using MRO 3.5 then a newer checkpoint of the repository is needed for `conquer`:
# install.packages("conquer", repos='https://mran.microsoft.com/snapshot/2020-08-28', Ncpus = 4)
## If using R 4.0 then `robustHD` and `MTE` needs to be compiled from Github:
# devtools::install_github("aalfons/robustHD")
# devtools::install_git("https://github.com/shaobo-li/MTE.git")
```

## Datasets

Most datasets are downloaded when used, run `experiments/regression/data.R` to download all datasets in advance.
The datasets in `experiments/data/retrive_*.R` can be acquired and pre-processed by running the respective `retrieve_*.R` scripts, but if not then the the corresponding datasets from the conference paper supplements will be downloaded instead.
