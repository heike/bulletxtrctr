#' Predict smooth from a fit
#'
#' @param y numeric vector
#' @param span The span of the loess fit
#' @export
#' @import assertthat
#' @examples
#' \dontrun{
#' library(ggplot2)
#' x <- seq(0, 6 * pi, by = .02)
#' y <- 5.5 * sin(x) + rnorm(length(x))
#'
#'
#' ggplot() +
#'   geom_point(aes(x = 1:length(y), y = y), shape = 1) +
#'   geom_line(aes(x = 1:length(y), y = smoothloess(y, .05)),
#'     color = "red"
#'   ) +
#'   geom_line(aes(x = 1:length(y), y = raw_sig_smooth(y, .05)),
#'     color = "blue"
#'   )
#' }
smoothloess <- function(y, span) {
  assert_that(is.numeric(y), is.numeric(span), length(y) > 10)
  dat <- data.frame(x = 1:length(y), y)
  lwp <- with(dat, loess(y ~ x, span = span))
  predict(lwp, newdata = dat)
}

#' Smooth the raw signature
#'
#' @param sig numeric vector of the raw signature
#' @param span width of the smoother, defaults to 0.03. XXX Should be
#' @param limits vector of the form c(min, max). Results will be limited to be
#'          between these values.
#' @return numeric vector of the same length as the input with the smoothed
#'           signature.
#' @export
#' @import assertthat
#' @examples
#' \dontrun{
#' library(ggplot2)
#' x <- seq(0, 6 * pi, by = .02)
#' y <- 5.5 * sin(x) + rnorm(length(x))
#'
#'
#' ggplot() +
#'   geom_point(aes(x = 1:length(y), y = y), shape = 1) +
#'   geom_line(aes(x = 1:length(y), y = smoothloess(y, .05)),
#'     color = "red"
#'   ) +
#'   geom_line(aes(x = 1:length(y), y = raw_sig_smooth(y, .05)),
#'     color = "blue"
#'   )
#' }
raw_sig_smooth <- function(sig, span = 0.03, limits = c(-5, 5)) {
  # bulletSmooth <- function(data, span = 0.03, limits = c(-5,5), id="bullet") {
  assert_that(is.numeric(limits))
  myspan <- ifelse(span > 1, span / length(sig), span)
  l30 <- smoothloess(sig, span = myspan[1])


  l30 <- pmin(max(limits), l30)
  l30 <- pmax(min(limits), l30)
  l30
}
