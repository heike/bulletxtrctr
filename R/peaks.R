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
#' @param plot If TRUE, show a plot
#' @return list of several objects:
#' @importFrom zoo rollapply
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#' library(ggplot2)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   x3p_clean = function(x) x %>%
#'       x3p_scale_unit(scale_by=10^6) %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' sig <- cc_get_signature(example_data$ccdata[[1]], example_data$grooves[[1]])
#' sig_peaks <- sig_get_peaks(sig$sig)
#' sig_peaks[1:6]
#' # Plot
#' sig_peaks$plot
#' }
sig_get_peaks <- function(sig, smoothfactor = 35, striae = TRUE, window = TRUE, plot = TRUE) {
  x <- NULL
  xmin <- NULL
  xmax <- NULL

  assert_that(
    is.numeric(sig), is.numeric(smoothfactor),
    is.logical(striae), is.logical(window)
  )

  smoothed <- rollapply(sig, smoothfactor, function(x) mean(x),
    fill = list(NA, NA, NA)
  )

  smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x),
    fill = list(NA, NA, NA)
  )

  find_maxs <- rollapply(smoothed_truefalse, 3, function(x) max(x) == x[2],
    fill = list(NA, NA, NA)
  )

  find_mins <- rollapply(smoothed_truefalse, 3, function(x) min(x) == x[2],
    fill = list(NA, NA, NA)
  )


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

  lines <- data.frame(
    xmin = extrema - c(firstval, diffs) / 3,
    xmax = extrema + c(diffs, lastval) / 3,
    type = type, extrema = extrema, heights = heights
  )
  dframe <- data.frame(
    x = 1:length(smoothed_truefalse),
    smoothed = smoothed_truefalse
  )
  if (plot) {
    p <- ggplot(data = dframe, aes(x = x, y = smoothed)) + geom_line() + theme_bw()
    if (window) {
      p <- p + geom_rect(aes(xmin = xmin, xmax = xmax),
        ymin = -6, ymax = 6,
        data = lines, colour = "grey60", alpha = 0.2,
        inherit.aes = FALSE
      )
    }
    if (striae) p <- p + geom_vline(xintercept = peaks, colour = "red")
    if (striae) p <- p + geom_vline(xintercept = valleys, colour = "blue")
    return(list(
      peaks = peaks, valleys = valleys, extrema = extrema,
      peaks.heights = peaks.heights, valleys.heights = valleys.heights,
      lines = lines, plot = p, dframe = dframe
    ))
  } else {
    return(list(
      peaks = peaks, valleys = valleys, extrema = extrema,
      peaks.heights = peaks.heights, valleys.heights = valleys.heights,
      lines = lines, plot = NULL, dframe = dframe
    ))
  }
}



#' Check a striae object is valid
#'
#' @param striae data frame with a striae object
#' @return TRUE if all conditions are met, error otherwise
#' @importFrom assertthat assert_that has_name
check_striae <- function(striae) {
  assert_that(
    is.data.frame(striae),
    has_name(striae, "xmin"), has_name(striae, "xmax"),
    has_name(striae, "type"), has_name(striae, "extrema"),
    has_name(striae, "heights"),
    is.numeric(striae$xmin), is.numeric(striae$xmax)
  )
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
#'          xmin, xmax, group, type, bullet, heights#' Check a striae object is valid
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
  land <- NULL

  check_striae(striae1)
  check_striae(striae2)

  lines1 <- striae1 %>% dplyr::mutate(land = 1)
  lines2 <- striae2 %>% dplyr::mutate(land = 2)

  lines <- rbind(lines1, lines2)
  lines <- lines[order(lines$xmin), ]
  ml <- tidyr::gather(lines, variable, value, c("xmin", "xmax"),
    factor_key = TRUE
  )
  ml <- ml[order(ml$value), ]
  ml$overlap <- c(1, -1)[as.numeric(ml$variable)]
  ml$gap <- cumsum(ml$overlap)

  idx <- which(ml$gap == 0)
  lines <- data.frame(
    xmin = ml$value[c(1, idx[-length(idx)] + 1)],
    xmax = ml$value[idx]
  )
  ml$group <- 0
  ml$group[c(1, idx[-length(idx)] + 1)] <- 1
  ml$group <- cumsum(ml$group)
  isMatch <- function(type, land) {
    if (length(unique(land)) != 2) return(FALSE)
    return(length(unique(type)) == 1)
  }
  groups <- ml %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      match = isMatch(type, land),
      size = n(),
      type = type[1],
      sdheights = sd(heights),
      heights = mean(heights)
    )
  lines$match <- as.vector(groups$match)
  lines$type <- as.vector(groups$type)
  lines$type[!lines$match] <- NA
  lines$meany <- with(lines, (xmin + xmax) / 2)
  lines$heights <- as.vector(groups$heights)
  lines$sdheights <- as.vector(groups$sdheights)
  lines
}
