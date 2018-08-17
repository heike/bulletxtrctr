#' Identify the location and the depth of peaks and valleys in a signature
#'
#' Adapted from the function `get_peaks`
#'
#' @param sig numeric vector of smoothed signature, padded with NAs at front
#'          and/or back.
#' @param smoothfactor set to default of 35. Smaller values will pick up on
#'          smaller changes in the crosscut.
#' @param striae If TRUE, show the detected striae on the plot
#' @param window If TRUE, show the window of the striae on the plot
#' @return list of several objects:
#' @importFrom zoo rollapply
#' @import ggplot2
#' @import assertthat
#' @export
sig_get_peaks <- function(sig, smoothfactor = 35, striae = TRUE, window = TRUE) {
  x <- NULL
  xmin <- NULL
  xmax <- NULL

  assert_that(is.numeric(sig), is.numeric(smoothfactor),
              is.logical(striae), is.logical(window))

  smoothed <- rollapply(sig, smoothfactor, function(x) mean(x),
                        fill = list(NA,NA,NA))
  smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x),
                                  fill = list(NA, NA, NA))

  find_maxs <- rollapply(smoothed_truefalse, 3, function(x) max(x) == x[2],
                         fill = list(NA,NA,NA))
  find_mins <- rollapply(smoothed_truefalse, 3, function(x) min(x) == x[2],
                         fill  = list(NA,NA,NA))


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

  lines <- data.frame(xmin = extrema - c(firstval, diffs)/3,
                      xmax = extrema + c(diffs, lastval)/3,
                      type = type, extrema = extrema, heights = heights)
  dframe <- data.frame(x = 1:length(smoothed_truefalse),
                       smoothed = smoothed_truefalse)

  p <- qplot(data = dframe, x = x, y = smoothed, geom = "line") + theme_bw()
  if (window) p <- p + geom_rect(aes(xmin = xmin, xmax = xmax),
                                 ymin = -6, ymax = 6,
                                 data = lines, colour = "grey60", alpha = 0.2,
                                 inherit.aes = FALSE)
  if (striae) p <- p + geom_vline(xintercept = peaks, colour = "red")
  if (striae) p <- p + geom_vline(xintercept = valleys, colour = "blue")

  return(list(peaks = peaks, valleys = valleys, extrema = extrema,
              peaks.heights = peaks.heights, valleys.heights = valleys.heights,
              lines = lines, plot = p, dframe = dframe))
}




#' Match striation marks across two aligned signatures
#'
#' `striae1` and `striae2` are data frames of previously identified peaks and
#' valleys returned by `sig_get_peaks`
#'
#' adapted from function `striation_identify` in the bulletr package, just used
#' internally. Not intended for public consumption.
#' @param striae1 data frame as returned from sig_get_peaks function.
#'          data frames are expected to have the following variables:
#'          xmin, xmax, group, type, bullet, heights
#' @param striae2 data frame as returned from sig_get_peaks function.
#'          data frames are expected to have the following variables:
#'          xmin, xmax, group, type, bullet, heights
#' @return data frame of the same form as lines1 and lines2, but with an
#'   additional variable of whether the striation marks are matches
#' @importFrom dplyr group_by %>% summarise
#' @importFrom tidyr gather
#' @importFrom stats sd
striation_identify_matches <- function(striae1, striae2) {
  group <- NULL
  type <- NULL
  bullet <- NULL
  heights <- NULL
  n <- NULL
  variable <- NULL
  value <- NULL

  assert_that(# striae1
    is.data.frame(striae1),
    has_name(striae1, "xmin"), has_name(striae1, "xmax"),
    has_name(striae1, "group"), has_name(striae1, "type"),
    has_name(striae1, "bullet"), has_name(striae1, "heights"),
    is.numeric(striae1$xmin), is.numeric(striae1$xmax)
  )
  assert_that(# striae2
    is.data.frame(striae2),
    has_name(striae2, "xmin"), has_name(striae2, "xmax"),
    has_name(striae2, "group"), has_name(striae2, "type"),
    has_name(striae2, "bullet"), has_name(striae2, "heights"),
    is.numeric(striae2$xmin), is.numeric(striae2$xmax)
  )

  lines1 <- striae1
  lines2 <- striae2

  lines <- rbind(lines1, lines2)
  lines <- lines[order(lines$xmin),]
  ml <- tidyr::gather(lines, variable, value, c("xmin", "xmax"),
                      factor_key = TRUE)
  ml <- ml[order(ml$value),]
  ml$overlap <- c(1,-1)[as.numeric(ml$variable)]
  ml$gap <- cumsum(ml$overlap)

  idx <- which(ml$gap == 0)
  lines <- data.frame(xmin = ml$value[c(1,idx[-length(idx)] + 1)],
                      xmax = ml$value[idx])
  ml$group <- 0
  ml$group[c(1, idx[-length(idx)] + 1)] <- 1
  ml$group <- cumsum(ml$group)
  isMatch <- function(type, bullet) {
    if (length(unique(bullet)) != 2) return(FALSE)
    return(length(unique(type)) == 1)
  }
  groups <- ml %>% group_by(group) %>% summarise(
    match = isMatch(type, bullet),
    size = n(),
    type = type[1],
    sdheights = sd(heights),
    heights = mean(heights))
  lines$match <- as.vector(groups$match)
  lines$type <- as.vector(groups$type)
  lines$type[!lines$match] <- NA
  lines$meany <- with(lines, (xmin + xmax)/2)
  lines$heights <- as.vector(groups$heights)
  lines$sdheights <- as.vector(groups$sdheights)
  lines
}
