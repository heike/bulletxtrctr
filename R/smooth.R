#' Predict smooth from a fit
#'
#' @param y numeric vector
#' @param span The span of the loess fit
#' @export
smoothloess <- function(y, span) {
  dat <- data.frame(x = 1:length(y), y)
  lwp <- with(dat, loess(y~x,span=span))
  predict(lwp, newdata = dat)
}

#' Smooth the raw signature
#'
#' @param sig numeric vector of the raw signature
#' @param span width of the smoother, defaults to 0.03. XXX Should be
#' @param limits vector of the form c(min, max). Results will be limited to be between these values.
#' @return numeric vector of the same length as the input with the smoothed signature.
#' @export
raw_sig_smooth <- function(sig, span = 0.03, limits = c(-5,5)) {
#bulletSmooth <- function(data, span = 0.03, limits = c(-5,5), id="bullet") {
  myspan = ifelse(span > 1, span / length(sig), span)
  l30 = smoothloess(sig, span = myspan[1])


  l30 <- pmin(max(limits), l30)
  l30 <- pmax(min(limits), l30)
  l30
}
