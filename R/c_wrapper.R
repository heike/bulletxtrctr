#' Wrapper function for compute_cross_corr
#' @useDynLib bulletxtrctr, .registration=TRUE
#' @param x numeric vector, the longer sequence
#' @param y numeric vector, the shorter sequence
#' @param min.overlap numeric scalar, set the length of the minimum overlapping part
compute_cross_corr <- function(x, y, min.overlap) .Call(compute_cross_corr_c, x, y, min.overlap)

#' Wrapper function for na_trim
#' @param x numeric vector
trim_na_count <- function(x) .Call(trim_na_c, x)

