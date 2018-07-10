#' Find the grooves of a bullet land
#'
#' @param bullet data frame with topological data in x-y-z format
#' @param method method to use for identifying grooves. Defaults to "rollapply"
#' @param smoothfactor The smoothing window to use - XXX the smoothing window seems to depend on the resolution at which the data has been collected.
#' @param adjust positive number to adjust the grooves - XXX should be expressed in microns rather than an index
#' @param groove_cutoff The index at which a groove cannot exist past - XXX this parameter should be expressed in microns rather than as an index to be able to properly deal with different resolutions
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param second_smooth Whether or not to smooth a second time
#' @export
#' @import ggplot2

cc_locate_grooves <- function(bullet, method = "rollapply", smoothfactor = 15, adjust = 10, groove_cutoff = 400, mean_left = NULL, mean_right = NULL, mean_window = 100) {
  bullet <- switch_xy(bullet)

  if (method == "quadratic") {
    grooves <- bulletr:::get_grooves_quadratic(bullet = bullet, adjust=adjust)
  }
  if (method == "rollapply") {
    # make sure there is only one x
    if (length(unique(bullet$x)) > 1) {
      message(sprintf("summarizing %d profiles by averaging across values\n", length(unique(bullet$x))))
      bullet <- bullet %>% group_by(y) %>% summarize(
        x = mean(x, na.rm = TRUE),
        value = mean(value, na.rm=TRUE)
      )
    }
    grooves <- bulletr:::get_grooves_rollapply(
      bullet = bullet,
      smoothfactor = smoothfactor,
      adjust = adjust,
      groove_cutoff = groove_cutoff,
      mean_left = mean_left,
      mean_right = mean_right,
      mean_window = mean_window,
      second_smooth = TRUE
    )
  }
  if (method == "middle") {
    grooves <- bulletr:::get_grooves_middle(
      bullet = bullet,
      middle = 75
    )
  }

  return(grooves)
}
