# Experiments

This directory contains the experiments used in the paper.

The supplements for the paper, which include some data, can be found [here](https://github.com/edahelsinki/slise/releases/download/v1.0/supplement.zip).


## Files

 - Helper functions
    - utils.R
    - lime.R
    - collect_results.R
 - Data downloading and preparation
    - data/retrieve_*.R
 - Experiments (some are designed to run on a cluster)
    - exp_*.R
 - Plotting results for the paper
    - plot_*.R


## Dependencies

For running all the experiments and gathering all the data the following
packages are needed, note that not all packages are needed individual
experiments:

 - MASS
 - MTE
 - robustHD
 - sparseLTSEigen
 - robustbase
 - glmnet
 - R.utils
 - ggplot2
 - dplyr
 - scales
 - abind
 - magick
 - lime
 - xtable
 - plyr
 - keras
 - R.matlab
 - tm
 - qdap
 - Matrix
 - SnowballC
 - e1071
 - randomForest
 - elmNNRcpp
 - latex2exp
 - pense
