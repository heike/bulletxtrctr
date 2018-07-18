#' Identify the number of maximum CMS between two signatures
#'
#' adapted from `bulletGetMaxCMS`
#' aligns two signatures, identifies peaks and valleys, matches striae, and counts longest run.
#' Should probably be split into four or five sub functions.
#' @param sig1 vector of smoothed first signature
#' @param sig2 vector of smoothed second signature
#' @param column The column which to consider for the signature
#' @param span positive number  for the smoothfactor to use for assessing peaks.
#' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
#' @export
sig_cms_max <- function(sig1, sig2, column = "resid", span = 35) {
  bullet <- NULL

#  t1 <- system.time({
  bAlign = sig_align(sig1[,column], sig2[,column])
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

#' Number of maximum consecutively matching striae
#'
#' @param match is a Boolean vector of matches/non-matches
#' @return an integer value of the maximum number of consecutive matches
#' @export
#' @examples
#' x <- rbinom(100, size = 1, prob = 1/3)
#' get_run(x == 1) # expected value for longest match is 3
#' get_longest_run(x==1)
get_longest_run <- function(match) {
  cmsTable <- get_run(match)
  as.numeric(rev(names(cmsTable)))[1]
}

#' Table of the number of consecutive matches
#'
#' @param match is a Boolean vector of matches/non-matches
#' @return a table of the number of the CMS and their frequencies
#' @export
#' @examples
#' x <- rbinom(100, size = 1, prob = 1/3)
#' get_runs(x == 1) # expected value for longest match is 3
get_run <- function(match) {
  # number of runs of different lengths

  y <- diff(match)
  # y is -1 if change from 1 to 0,
  #       0 if unchanged
  #       1 if change from 0 to 1
  w <- c(0, y)[match][-1]

  z <- which(w == 1)
  z <- c(0,z,length(match[match]))

  return(table(diff(z)))
}

