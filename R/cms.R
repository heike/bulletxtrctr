#' Identify the number of maximum CMS between two signatures
#'
#' adapted from `bulletGetMaxCMS`
#' aligns two signatures, identifies peaks and valleys, matches striae, and counts longest run.
#' Should probably be split into four or five sub functions.
#' @param sig1 vector of smoothed first signature
#' @param sig2 vector of smoothed second signature
#' @param span positive number  for the smoothfactor to use for assessing peaks.
#' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
#' @export
sig_cms_max <- function(sig1, sig2, span = 35) {
  bullet <- NULL

#  t1 <- system.time({
  bAlign = sig_align(sig1, sig2)
#  })
#  browser()

  sigX <- bAlign$bullet
  sigX$sig <- sigX$val


#    browser()
  peaks1 <- sig_get_peaks(sigX$sig1, smoothfactor = span)
  peaks2 <- sig_get_peaks(sigX$sig2, smoothfactor = span)
#  peaks1 <- sig_get_peaks(sig1[,column], smoothfactor = span)
#  peaks2 <- bulletr::get_peaks(subset(sigX, bullet == b12[2]), column = column, smoothfactor = span)

  #qplot(x=y, y=resid, geom="line", colour=bullet, data=sigX, group=bullet) +
  #    theme_bw() +
  #    geom_rect(data=peaks1$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25)) +
  #    geom_rect(data=peaks2$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25))

#  b12 <- unique(sigX$bullet)

#  peaks1$lines$bullet <- b12[1]
#  peaks2$lines$bullet <- b12[2]

  peaks1$lines$bullet <- "sig1"
  peaks2$lines$bullet <- "sig2"

  lines <- striation_identify_matches(peaks1$lines, peaks2$lines)

  #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=sigX, group=bullet) +
  #     theme_bw() +
  #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
  #     ylim(c(-6,6)) +
  #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
  #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE)

  maxCMS <- get_longest_run(lines$match==TRUE)
  list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=sigX)
}

#' Length of the longest run of TRUEs
#'
#' Identifies the length of the longest run of TRUEs in Boolean vector `x`.
#' used to be `maxCMS`
#' @param x  Boolean vector
#' @return an integer value of the length of the longest run of TRUE values
#' @export
#' @examples
#' x <- rbinom(100, size = 1, prob = 1/3)
#' get_run(x == 1) # expected value for longest match is 3
#' get_longest_run(x==1)
get_longest_run <- function(x) {
  runTable <- get_runs(x)
  as.numeric(rev(names(runTable)))[1]
}

#' Table of the number of runs
#'
#' Identify the length of runs (of values TRUE) and their frequencies.
#' @param x Boolean vector
#' @return a table of the number of runs of TRUEs
#' @export
#' @examples
#' x <- rbinom(100, size = 1, prob = 1/3)
#' get_runs(x == 1) # expected value for longest run is 3
#' get_runs(x == 0) # expected value for longest run is 6
get_runs <- function(x) {
  # number of runs of different lengths

  if (!is.logical(x)) {
    warning("Converting x to a logical vector")
    x <- as.logical(x)
  }

  y <- diff(x)
  # y is -1 if change from 1 to 0,
  #       0 if unchanged
  #       1 if change from 0 to 1
  w <- c(0, y)[x][-1]

  z <- which(w == 1)
  z <- c(0,z,length(x[x]))

  return(table(diff(z)))
}

