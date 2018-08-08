#' Fit a loess curve to a bullet data frame
#'
#' A loess regression is fit to the surface measurements and residuals are calculated.
#' The most extreme 0.25% of residuals are filtered from further consideration.
#' The result is called the raw signature of the bullet land.
#' Adapted from `fit_loess` in `bulletr`
#' @param ccdata The crosscut as returned from x3p_to_df, grooves need to be removed ahead of time
#' @param span The span to use for the loess regression
#' @return a list of a data frame of the original bullet measurements extended by loess fit, residuals, and standard errors and two plots: a plot of the fit, and a plot of the bullet's land signature.
#' @importFrom stats fitted predict quantile loess resid
#' @export
cc_fit_loess <- function(ccdata, span = 0.75) {
  value <- NULL
  x <- NULL

  # HH Mar 22: we should use lowess rather than loess

  my.loess <- loess(value ~ x, data = ccdata, span = span)
  ccdata$fitted <- fitted(my.loess)
  ccdata$raw_sig <- resid(my.loess)
  ccdata$se <- predict(my.loess, se=TRUE)$se.fit

  # filter out most extreme residuals
  ccdata$abs_resid <-  abs(ccdata$raw_sig)
  cutoff <- quantile(ccdata$abs_resid, probs = c(0.9975))
  ccdata$chop <- ccdata$abs_resid > cutoff

  return(ccdata)
}



#' Use a gaussian filter on bullet data frame
#'
#' A gaussian filter is fit to the surface measurements and residuals are calculated.
#' The most extreme 0.25\% of residuals are filtered from further consideration.
#' The result is called the raw signature of the bullet land.
#' @param ccdata The crosscut as returned from x3p_to_df
#' @param span The size, in microns, of the smoothing window. Defaults to 600, which is 24\% of 2500 microns; approximately the same size as a 9mm land.
#' @return a list of a data frame of the original bullet measurements extended by gaussian filtration, residuals, and two plots: a plot of the fit, and a plot of the bullet's land signature.
#' @importFrom stats median
#' @importFrom smoother smth
#' @export
cc_fit_gaussian <- function(ccdata, span = 600) {
  value <- NULL
  x <- NULL

  dx <- median(diff(ccdata$x), na.rm = T)
  window <- span/dx

  # Alternative to loess fit

  gsmooth <- smoother::smth(x = ccdata$value, method = 'gaussian', window = window, tails = T, na.rm = T)
  ccdata$fitted <- gsmooth
  ccdata$raw_sig <- ccdata$value - ccdata$fitted
  ccdata$se <- NA

  # filter out most extreme residuals
  ccdata$abs_resid <-  abs(ccdata$raw_sig)
  cutoff <- quantile(ccdata$abs_resid, probs = c(0.9975))
  ccdata$chop <- ccdata$abs_resid > cutoff

  return(ccdata)
}
