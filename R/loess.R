#' Fit a loess curve to a bullet data frame
#'
#' A loess regression is fit to the surface measurements and residuals are calculated.
#' The most extreme 0.25% of residuals are filtered from further consideration.
#' The result is called the raw signature of the bullet land.
#' @param ccdata The crosscut as returned from x3p_to_df
#' @param span The span to use for the loess regression
#' @return a list of a data frame of the original bullet measurements extended by loess fit, residuals, and standard errors and two plots: a plot of the fit, and a plot of the bullet's land signature.
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

#' Fit a loess curve to a bullet data frame
#'
#' A loess regression is fit to the surface measurements and residuals are calculated.
#' The most extreme 0.25% of residuals are filtered from further consideration.
#' The result is called the raw signature of the bullet land.
#' @param ccdata The crosscut as returned from x3p_to_df
#' @param span The span to use for the loess regression
#' @return a list of a data frame of the original bullet measurements extended by loess fit, residuals, and standard errors and two plots: a plot of the fit, and a plot of the bullet's land signature.
#' @export
cc_fit_gaussian <- function(ccdata, span = 0.75) {
  value <- NULL
  x <- NULL

  # HH Mar 22: we should use lowess rather than loess

  my.loess <- lowess(ccdata$x, ccdata$value, f = span)
  ccdata$fitted <- fitted(my.loess)
  ccdata$raw_sig <- resid(my.loess)
  ccdata$se <- predict(my.loess, se=TRUE)$se.fit

  # filter out most extreme residuals
  ccdata$abs_resid <-  abs(ccdata$raw_sig)
  cutoff <- quantile(ccdata$abs_resid, probs = c(0.9975))
  ccdata$chop <- ccdata$abs_resid > cutoff

  return(ccdata)
}
