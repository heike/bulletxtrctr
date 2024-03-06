
#' Align two surface cross cuts according to maximal correlation
#'
#' The bullet with the first name serves as a reference, the second bullet is
#' shifted.
#' @param sig1 vector of first signature
#' @param sig2 vector of second signature
#' @param min.overlap additional parameter passed on to get_ccf
#' @return list consisting of
#'           a) the maximal cross correlation,
#'           b) the lag resulting in the highest cross correlation, and
#'           c) same data frame as input, but y vectors are aligned for
#'              maximal correlation
#'           d) a vector of cross-correlation values
#' @export
#' @importFrom assertthat assert_that
#' @importFrom zoo na.trim
#' @importFrom stats cor
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(
#'     Bullet1 = c(hamby252demo$bullet1[2]),
#'     Bullet2 = c(hamby252demo$bullet2[4])
#'   ),
#'   x3p_clean = function(x) x %>%
#'       x3p_scale_unit(scale_by=10^6) %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' sig_align(example_data$sigs[[1]]$sig, example_data$sigs[[2]]$sig)
#' }
sig_align <- function(sig1, sig2, min.overlap = NULL) {
  assert_that(is.numeric(sig1), is.numeric(sig2))

  sig1 <- na.trim(sig1)
  sig2 <- na.trim(sig2)

  n1 <- length(sig1)
  n2 <- length(sig2)

  # make sure that all the missing values are removed at either end of the
  # signatures before aligning them
  if (is.null(min.overlap)) min.overlap = round(0.75 * min(length(sig1), length(sig2)))
  # assume y is the long vector, x is the short vector. If not, switch the
  # vectors around
  if (n1 < n2) {
    x <- sig1
    y <- sig2
  } else {
    x <- sig2
    y <- sig1
  }

  cors <- get_ccf(x, y, min.overlap = min.overlap)

  # do some padding at the front
  lag <- cors$lag[which.max(cors$ccf)]
  if (lag < 0) {
    x <- c(rep(NA, abs(lag)), x)
  }
  if (lag > 0) {
    y <- c(rep(NA, lag), y)
  }

  # at the back
  delta <- length(x) - length(y)
  if (delta < 0) x <- c(x, rep(NA, abs(delta)))
  if (delta > 0) y <- c(y, rep(NA, delta))

  # switch back
  if (n1 < n2) {
    dframe0 <- data.frame(x = 1:length(x), sig1 = x, sig2 = y)
  } else {
    dframe0 <- data.frame(x = 1:length(x), sig1 = y, sig2 = x)
  }

  maxcor <- max(cors$ccf, na.rm = TRUE)

  # dfcor <- cor(dframe0$sig1, dframe0$sig2, use = "pairwise")
  # if (maxcor != dfcor) browser()

  list(ccf = maxcor, lag = lag, lands = dframe0, cors = cors)
}

#' Check align output
#'
#' @param x output from sig_align
#' @return TRUE or error
#' @importFrom assertthat assert_that has_name
check_align <- function(x) {
  assert_that(
    has_name(x, "ccf"),
    has_name(x, "lag"),
    has_name(x, "lands")
  )
  assert_that(
    has_name(x$lands, "x"),
    has_name(x$lands, "sig1"),
    has_name(x$lands, "sig2")
  )
}


#' Cross correlation function between two vectors
#'
#' Calculate the lagged correlation between numeric vectors x and y.
#' Vectors x and y are assumed to be captured at the same resolution and,
#' similarly,
#' successive values in x and y are assumed to be equi-distant.
#' Missing values are allowed in each vector, correlations are calculated
#' based on the complete cases.
#' @details
#' This version of the cross correlation function is different from the `stats`
#' implementation of `ccf` in two ways:
#'
#'   1. We consider the full region of correlations between vectors x and y as
#' specified by min.overlap rather than just the overlap. The two vectors can
#' be of very different
#' length (e.g. when y is just a snippet recovered from a crime scene and x is
#' from the full length object in the lab).
#'   2. We do not use a Fourier transformation to calculate cross-correlation.
#' This makes the evaluation slower, but prevents any edge effects.
#' @param x vector, assumption is that x is longer than y
#' @param y vector
#' @param min.overlap integer value: what is the minimal number of values
#'          between x and y that should be considered?
#' @return list with ccf values and lags
#' @importFrom stats na.omit
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' library(dplyr)
#' x <- runif(20)
#' get_ccf(x, lead(x, 5))
#' get_ccf(x, lag(x, 5), min.overlap = 3)
#' x <- runif(100)
#' get_ccf(x[45:50], x, min.overlap = 6)
get_ccf <- function(x, y, min.overlap = round(0.1 * max(length(x), length(y)))) {
  x <- as.vector(unlist(x))
  y <- as.vector(unlist(y))
  nx <- length(x)
  ny <- length(y)

  assert_that(is.numeric(x), is.numeric(y))
  assert_that(nx > 0, ny > 0, nx <= ny)

  xx <- c(rep(NA, ny - min.overlap), x, rep(NA, ny - min.overlap))
  yy <- c(y, rep(NA, length(xx) - ny))

  lag.max <- length(yy) - length(y)
  lags <- 0:lag.max

  cors <- sapply(lags, function(lag) {
    cor(xx, lag(yy, lag), use = "pairwise.complete")
  })
  ns <- sapply(lags, function(lag) {
    dim(na.omit(cbind(xx, lag(yy, lag))))[1]
  })
  cors[ns < min.overlap] <- NA

  lag <- lags - (ny - min.overlap)
  return(list(lag = lag, ccf = cors))
}



#' #' Align two surface cross cuts according to maximal correlation
#' #'
#' #' The bullet with the first name serves as a reference, the second bullet is shifted.
#' #' Function copied from `bulletAlign`
#' #' @param sig1 vector of first signature
#' #' @param sig2 vector of second signature
#' #' @return list consisting of a) the maximal cross correlation, b) the lag resulting in the highest cross correlation, and c) same data frame as input, but y vectors are aligned for maximal correlation
#' #' @export
#' #' @importFrom zoo na.trim
#' #' @importFrom stats cor
#' sig_align <- function (sig1, sig2)  {
#'   stopifnot(is.numeric(sig1), is.numeric(sig2))
#'
#'   sig1 <- na.trim(sig1)
#'   sig2 <- na.trim(sig2)
#' # browser()
#'   subLOFx1 <- data.frame(x = 1:length(sig1), val = sig1, bullet="land-1")
#'   subLOFx2 <- data.frame(x = 1:length(sig2), val = sig2, bullet="land-2")
#'
#'   whichmin <- which.min(c(length(sig1), length(sig2)))
#'   shorter <- list(subLOFx1$val, subLOFx2$val)[[whichmin]]
#'   longer <- list(subLOFx1$val, subLOFx2$val)[[3 - whichmin]]
#'
#'   mylagmax <- 250
#'
#'   longer_na <- c(rep(NA, mylagmax), longer, rep(NA, mylagmax))
#'
#'   mycors <- NULL
#'   for (i in 1:(length(longer_na) - length(shorter))) {
#'     longersub <- longer_na[i:(i + length(shorter) - 1)]
#'
#'     corval <- cor(shorter, longersub, use = "pairwise.complete.obs")
#'
#'     mycors <- c(mycors, corval)
#'   }
#'
#'   lag <- which.max(mycors) - mylagmax
#'
#'   mydat <- if (whichmin == 1) subLOFx1 else subLOFx2
#'   mydat2 <- if (whichmin == 1) subLOFx2 else subLOFx1
#'
#'   if (lag < 0)
#'     mydat2$x <- mydat2$x - lag -1
#'   else
#'     mydat$x <- mydat$x + lag
#'
#'
#'
#'   max_x <- max(max(mydat$x), max(mydat2$x))
#'
#'   dframe <- data.frame(x = 1:max_x)
#'   mydat$sig1 <- mydat$val
#'   dframe <- dframe %>% left_join(mydat[, c("x", "sig1")], by="x")
#'   mydat2$sig2 <- mydat2$val
#'   dframe <- dframe %>% left_join(mydat2[, c("x", "sig2")], by="x")
#'
#' #  dframe %>%
#' #    ggplot(aes(x = x, y = sig1)) + geom_line(colour="blue") +
#' #    geom_line(aes(y = sig2), colour = "orange")
#'
#'
#' #
#'   list(ccf = max(mycors, na.rm = TRUE), lag = lag, bullets = dframe)
#' }
#'
#' # Browse[1]> t1
#' # user  system elapsed
#' #  0.036   0.002   0.040
