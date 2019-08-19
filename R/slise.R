#' @useDynLib slise
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("slise", libpath)
}
