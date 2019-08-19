# SLISE

R implementation of the SLISE algorithm.  
For more details see the paper "Sparse Robust Regression for Explaining
Classifiers" by A. Björklund, A. Henelius, E. Oikarinen, K. Kallonen, and
K. Puolamäki submitted to ECML PKDD 2019.


## Example

In order to use SLISE you need to have your data in a numerical matrix (or
something that can be cast to a matrix), and the response as a numerical vector.
Below is an example of SLISE being used for robust regression:

```{R}
source("slise.R")
source("experiments/utils.R")
data <- data_pox("fpox", "all")
slise <- slise.fit(X=data$X, Y=data$Y, epsilon=0.1, lambda=0)
title <- sprintf("SLISE as Robust Regression   [Intercept = %.0f, Smallpox = %.2f]",
    slise$coefficients[1], slise$coefficients[2])
plot(slise, labels=c("Smallpox", "All Deaths"), title=title)
```
![Example Plot 1](results/ex1.jpg)


SLISE can also be used to explain an opaque classifiers:

```{R}
source("slise.R")
source("experiments/utils.R")
set.seed(42)
emnist <- data_emnist(10000, classifier="digits2")
slise <- slise.explain(emnist$X, emnist$Y, 3, epsilon = 0.1, lambda = 2, logit = TRUE)
show(slise, "image", class_labels=c("not 2", "is 2"), title="Using SLISE to explain a handwritten digit")
```
![Example Plot 1](results/ex2.jpg)


## For Reviewers
  
.==================================================  
This is unpublished software for review purposes only.  
These files may not be used for other purposes or distributed.  
.==================================================  

This is supplementary material for the paper "Sparse Robust Regression for
Explaining Classifiers". The code for the experiments, data, and plots in the
paper can be found in the `experiments` subdirectory, while the implementation
of the SLISE algorithm can be found in the root directory. All the data and
models used in the paper are included in the zip-file so running the
`experiments/data/retrieve_*.R` scripts are not strictly necessary. Also the
cached experiment results are included so the `experiments/*_plots.R` scripts
can be run immediately, without first running the experiments. All R-scripts
(also those in subdirectories) are designed to be sourced from the root
directory.


## Dependencies

These R-packages must be installed to run SLISE:

 - Rcpp
 - lbfgs

For the built-in visualisation the following are (optionally) needed:

 - ggplot2
 - scatterplot3d
 - grid
 - gridExtra
 - reshape2
 - crayon
 - wordcloud
