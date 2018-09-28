#' Check loess or gaussian curve fit object
#'
#' @param x output from cc_fit_loess or cc_fit_gaussian
#' @return TRUE or error
#' @importFrom assertthat assert_that has_name
check_loess_fit <- function(x) {
  assert_that(
    has_name(x, "fitted"), has_name(x, "raw_sig"),
    has_name(x, "se"), has_name(x, "abs_resid"),
    has_name(x, "chop")
  )
}

#' Fit a loess curve to a bullet data frame
#'
#' A loess regression is fit to the surface measurements and residuals are
#' calculated.
#' The most extreme 0.25% of residuals are filtered from further consideration.
#' The result is called the raw signature of the bullet land.
#' Adapted from `fit_loess` in `bulletr`
#' @param ccdata The crosscut as returned from x3p_to_df, grooves need to be
#'          removed ahead of time
#' @param span The span to use for the loess regression
#' @return a list of a data frame of the original bullet measurements extended
#'           by loess fit, residuals, and standard errors and two plots: a plot
#'           of the fit, and a plot of the bullet's land signature.
#' @importFrom stats fitted predict quantile loess resid
#' @import assertthat
#' @export
#' @examples
#' library(dplyr)
#' ccdata <- data_frame(
#'   x = seq(0, 6, .001),
#'   value = 10 - (3 - x)^2 + rnorm(length(x), sd = .25)
#' )
#' cc_fit_loess(ccdata = ccdata)
cc_fit_loess <- function(ccdata, span = 0.75) {
  value <- NULL
  x <- NULL

  check_ccdata(ccdata)
  assert_that(is.numeric(span))

  # HH Mar 22: we should use lowess rather than loess

  my.loess <- loess(value ~ x, data = ccdata, span = span)
  pred <- predict(my.loess, newdata = ccdata, se=TRUE)
  ccdata$fitted <- pred$fit
  ccdata$raw_sig <-  ccdata$value - ccdata$fitted
  ccdata$se <- pred$se.fit

  # filter out most extreme residuals
  ccdata$abs_resid <- abs(ccdata$raw_sig)
  cutoff <- quantile(ccdata$abs_resid, probs = c(0.9975), na.rm=TRUE)
  ccdata$chop <- ccdata$abs_resid > cutoff

  return(ccdata)
}



#' Use a gaussian filter on bullet data frame
#'
#' A gaussian filter is fit to the surface measurements and residuals are
#' calculated.
#' The most extreme 0.25\% of residuals are filtered from further consideration.
#' The result is called the raw signature of the bullet land.
#' @param ccdata The crosscut as returned from x3p_to_df
#' @param span The size, in microns, of the smoothing window. Defaults to 600,
#'          which is 24\% of 2500 microns; approximately the same size as a
#'          9mm land.
#' @return a list of a data frame of the original bullet measurements extended
#'           by gaussian filtration, residuals, and two plots: a plot of the
#'           fit, and a plot of the bullet's land signature.
#' @importFrom stats median
#' @importFrom smoother smth
#' @import assertthat
#' @export
#' @examples
#' library(dplyr)
#' ccdata <- data_frame(
#'   x = seq(0, 6000, 1),
#'   value = 10 - (3 - x / 1000)^2 + rnorm(length(x), sd = .25)
#' )
#' cc_fit_gaussian(ccdata = ccdata)
cc_fit_gaussian <- function(ccdata, span = 600) {
  value <- NULL
  x <- NULL

  check_ccdata(ccdata)
  assert_that(is.numeric(span))

  dx <- median(diff(ccdata$x), na.rm = T)
  window <- span / dx

  # Alternative to loess fit

  gsmooth <- smoother::smth(
    x = ccdata$value, method = "gaussian",
    window = window, tails = T, na.rm = T
  )
  ccdata$fitted <- gsmooth
  ccdata$raw_sig <- ccdata$value - ccdata$fitted
  ccdata$se <- NA

  # filter out most extreme residuals
  ccdata$abs_resid <- abs(ccdata$raw_sig)
  cutoff <- quantile(ccdata$abs_resid, probs = c(0.9975))
  ccdata$chop <- ccdata$abs_resid > cutoff

  return(ccdata)
}
