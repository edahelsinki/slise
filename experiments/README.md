# Experiments

The __explanations__ directory contains explanation experiments and the __data__ directory contains the data preprocessing (and model training) scripts.

## Dependencies

For running all the experiments and gathering all the data the following
packages are needed, note that not all packages are needed for individual
experiments:

- ggplot2
- xtable
- R.matlab
- tm
- Matrix
- e1071
- randomForest
- elmNNRcpp
- datasets
- reticulate
- keras
- tensorflow

### Reticulate dependencies

These are python packages that are accessed through reticulate:

- `keras::install_keras()`
- `tensorflow::install_tensorflow()`
- `reticulate::py_install("lime")`
- `reticulate::py_install("shap")`
