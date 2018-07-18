#' Identify the location and the depth of peaks and valleys in a signature
#'
#' Adapted from the function `get_peaks`
#'
#' @param sig numeric vector of smoothed signature, padded with NAs at front and/or back.
#' @param smoothfactor set to default of 35. Smaller values will pick up on smaller changes in the crosscut.
#' @param striae If TRUE, show the detected striae on the plot
#' @param window If TRUE, show the window of the striae on the plot
#' @return list of several objects:
#' @importFrom zoo rollapply
#' @import ggplot2
#' @export
sig_get_peaks <- function(sig, smoothfactor = 35, striae = TRUE, window = TRUE) {
  x <- NULL
  xmin <- NULL
  xmax <- NULL
#browser()
  smoothed <- rollapply(sig, smoothfactor, function(x) mean(x), fill=list(NA,NA,NA))
  smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x), fill = list(NA, NA, NA))

  find_maxs <- rollapply(smoothed_truefalse, 3, function(x) max(x)==x[2], fill=list(NA,NA,NA))
  find_mins <- rollapply(smoothed_truefalse, 3, function(x) min(x)==x[2], fill=list(NA,NA,NA))


  peaks <- which(find_maxs)
  valleys <- which(find_mins)

  peaks.heights <- sig[peaks]
  valleys.heights <- sig[valleys]

  # adding on some extra stats
  extrema <- c(peaks, valleys)
  heights <- c(peaks.heights, valleys.heights)
  type <- c(rep(1, length(peaks)), rep(-1, length(valleys)))
  idx <- order(extrema)
  extrema <- extrema[idx]
  heights <- heights[idx]
  type <- type[idx]
  diffs <- diff(extrema)

  firstval <- diffs[1]
  if (is.na(firstval)) firstval <- 0
  lastval <- diffs[length(diffs)]
  if (length(lastval) == 0) lastval <- 0

  lines <- data.frame(xmin = extrema-c(firstval,diffs)/3,
                      xmax = extrema+c(diffs,lastval)/3,
                      type = type, extrema = extrema, heights = heights)
  dframe <-
    data.frame(
      x=1:length(smoothed_truefalse),
      smoothed=smoothed_truefalse
      )
  p <- qplot(data=dframe, x=x, y=smoothed, geom = "line") +
    theme_bw()
  if (window) p <- p + geom_rect(aes(xmin=xmin, xmax=xmax), ymin=-6, ymax=6, data=lines, colour="grey60", alpha=0.2, inherit.aes = FALSE)
  if (striae) p <- p + geom_vline(xintercept = peaks, colour = "red")
  if (striae) p <- p + geom_vline(xintercept = valleys, colour = "blue")

  return(list(peaks = peaks, valleys = valleys, extrema = extrema,
              peaks.heights = peaks.heights, valleys.heights = valleys.heights,
              lines=lines, plot = p, dframe = dframe))
}




