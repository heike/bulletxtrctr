#' Identify the number of maximum CMS between two signatures
#'
#' adapted from `bulletGetMaxCMS`
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

  lines <- bulletr::striation_identify(peaks1$lines, peaks2$lines)

  #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=sigX, group=bullet) +
  #     theme_bw() +
  #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
  #     ylim(c(-6,6)) +
  #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
  #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE)

  maxCMS <- bulletr::maxCMS(lines$match==TRUE)
  list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=sigX)
}
