#' Internal function to plot crosscut + grooves
#'
#' @param land data.frame with columns x and value
#' @param grooves numeric vector of length 2 identifying both grooves.
#'        If only one groove is identified, the other should be NA
#' @import assertthat
#' @import ggplot2
#' @return a ggplot2 object
grooves_plot <- function(land, grooves) {
  assert_that(has_name(land, "x"))
  assert_that(has_name(land, "value"))
  assert_that(is.numeric(grooves), sum(!is.na(grooves)) >= 1)
  x <- value <- NULL

  ggplot(aes(x = x, y = value), data = land) + geom_line(size = .5) + theme_bw() +
    geom_vline(xintercept = grooves[1], colour = "blue") +
    geom_vline(xintercept = grooves[2], colour = "blue")
}


#' Find the grooves of a bullet land
#'
#' @param ccdata data frame of the crosscut. Data frame needs location x and
#'          measured values as `value`. If multiple crosscuts are to be
#'          considered, include a variable y and use as a key.
#' @param method method to use for identifying grooves. Defaults to "rollapply"
#' @param smoothfactor The smoothing window to use - XXX the smoothing window
#'          seems to depend on the resolution at which the data has been
#'          collected.
#' @param adjust positive number to adjust the grooves - XXX should be
#'          expressed in microns rather than an index
#'          (not used for method = "middle")
#' @param groove_cutoff The index at which a groove cannot exist past - XXX
#'          this parameter should be expressed in microns rather than as an
#'          index to be able to properly deal with different resolutions
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param return_plot Return plot of grooves?
#' @param ... parameters passed on to specific groove location methods
#' @export
#' @import assertthat
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'     x3pheader_to_microns %>%
#'     rotate_x3p(angle = -90) %>%
#'     y_flip_x3p()
#' )
#'
#' cc_locate_grooves(example_data$ccdata[[1]], method = "rollapply",
#'                   adjust = 30, return_plot = T)
#' cc_locate_grooves(example_data$ccdata[[1]], method = "middle",
#'                   adjust = 30, return_plot = T)
#' cc_locate_grooves(example_data$ccdata[[1]], method = "quadratic",
#'                   adjust = 30, return_plot = T)
#' }
cc_locate_grooves <- function(ccdata, method = "rollapply", smoothfactor = 15,
                              adjust = 10, groove_cutoff = 400,
                              mean_left = NULL, mean_right = NULL, mean_window = 100,
                              return_plot = F, ...) {
  # TODO add documentation for different groove options, either here or by exporting the individual functions and documenting the parameters there.

  x <- y <- value <- NULL
  land <- ccdata

  assert_that(has_name(land, "x"), has_name(land, "y"), has_name(land, "value"))
  assert_that(method %in% c("quadratic", "rollapply", "middle"))
  assert_that(is.numeric(smoothfactor), is.numeric(adjust), is.numeric(groove_cutoff),
              is.logical(return_plot))

  if (method == "quadratic") {
    # grooves <- get_grooves_quadratic(bullet = bullet, adjust = adjust)
    grooves <- get_grooves_quadratic(x = land$x, value = land$value,
                                     adjust = adjust, return_plot = return_plot)
  }
  if (method == "rollapply") {
    # make sure there is only one x
    if (length(unique(land$y)) > 1) {
      message(sprintf("summarizing %d profiles by averaging across values\n",
                      length(unique(land$x))))
      land <- land %>% group_by(x) %>% summarize(
        y = mean(y, na.rm = TRUE),
        value = mean(value, na.rm = TRUE)
      )
    }
    grooves <- get_grooves_rollapply(
      x = land$x,
      value = land$value,
      smoothfactor = smoothfactor,
      adjust = adjust,
      groove_cutoff = groove_cutoff,
      mean_left = mean_left,
      mean_right = mean_right,
      mean_window = mean_window,
      second_smooth = TRUE,
      return_plot = return_plot
    )
  }
  if (method == "middle") {
    middle <- 75
    if ("middle" %in% names(list(...))) {
      middle <- list(...)$middle
    }
    grooves <- get_grooves_middle(x = land$x, value = land$value,
                                  middle = middle,
                                  return_plot = return_plot)
  }

  return(grooves)
}


#' Use the center of a crosscut
#'
#' @param x numeric vector of locations in microns
#' @param value numeric vector of surface measurements in microns
#' @param middle middle percent to use for the identification
#' @param return_plot return plot?
#' @return list of groove vector and plot of crosscut, if return_plot is true
#' @import assertthat
#' @export
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'     x3pheader_to_microns %>%
#'     rotate_x3p(angle = -90) %>%
#'     y_flip_x3p()
#' )
#'
#' get_grooves_middle(example_data$ccdata[[1]]$x,
#'                    example_data$ccdata[[1]]$value,
#'                    return_plot = T)
#' cc_locate_grooves(example_data$ccdata[[1]], method = "middle",
#'                   return_plot = T)
#' }
get_grooves_middle <- function(x, value, middle = 75, return_plot = F) {
  assert_that(is.numeric(x), is.numeric(value), is.numeric(middle),
              is.logical(return_plot))
  assert_that(middle >= 0, middle <= 100)

  land <- data.frame(x = x, value = value)
  groove <- quantile(land$x,
                     probs = c((100 - middle)/200, (100 + middle)/200))

  if (return_plot) {
    return(list(groove = groove,
                plot = grooves_plot(land = land, grooves = groove)))
  } else {
    return(list(groove = groove))
  }
}

#' Quadratic fit to find groove locations
#'
#' Use a robust fit of a quadratic curve to find groove locations
#' @param x numeric vector of locations (in microns)
#' @param value numeric values of surface measurements in microns
#' @param adjust positive number to adjust the grooves
#' @param return_plot return plot of grooves?
#' @return list of groove vector and plot of crosscut with shoulder locations
#' @importFrom MASS rlm
#' @export
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'     x3pheader_to_microns %>%
#'     rotate_x3p(angle = -90) %>%
#'     y_flip_x3p()
#' )
#'
#' get_grooves_quadratic(example_data$ccdata[[1]]$x,
#'                       example_data$ccdata[[1]]$value,
#'                       adjust = 30, return_plot = T)
#' cc_locate_grooves(example_data$ccdata[[1]], method = "quadratic",
#'                   adjust = 30, return_plot = T)
#' }
get_grooves_quadratic <- function(x, value, adjust, return_plot = F) {

  assert_that(is.numeric(x), is.numeric(value), is.numeric(adjust),
              is.logical(return_plot))

  land <- data.frame(x = x, value = value)

  lm0 <- rlm(value~poly(x,2), data = land, maxit = 100)
  land$pred <- predict(lm0, newdata = land)

  land$absresid <- with(land, abs(value - pred))
  absresid90 <- NULL
  land$absresid90 <- with(
    land, absresid > 4*median(land$absresid, na.rm = TRUE))

  groove <- range(filter(land, !absresid90)$x) + c(adjust, -adjust)

  if (return_plot) {
    return(list(groove = groove,
                plot = grooves_plot(land = land, grooves = groove)))
  } else {
    return(list(groove = groove))
  }
}

#' Using rollapply to find grooves in a crosscut
#'
#' @param x numeric vector of locations (in microns)
#' @param value numeric values of surface measurements in microns
#' @param smoothfactor The smoothing window to use
#' @param adjust positive number to adjust the grooves
#' @param groove_cutoff The index at which a groove cannot exist past
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param second_smooth Whether or not to smooth a second time
#' @param which_fun Which function to use in the rollapply statement
#' @param return_plot return plot of grooves?
#' @export
#' @import assertthat
#' @importFrom zoo rollapply
#' @importFrom zoo na.fill
#' @importFrom utils head tail
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'     x3pheader_to_microns %>%
#'     rotate_x3p(angle = -90) %>%
#'     y_flip_x3p()
#' )
#'
#' get_grooves_rollapply(example_data$ccdata[[1]]$x,
#'                       example_data$ccdata[[1]]$value,
#'                       adjust = 30, return_plot = T)
#' cc_locate_grooves(example_data$ccdata[[1]], method = "rollapply",
#'                   adjust = 30, return_plot = T)
#' }
get_grooves_rollapply <- function(x, value, smoothfactor = 15, adjust = 10,
                                  groove_cutoff = 400, mean_left = NULL,
                                  mean_right = NULL, mean_window = 100,
                                  second_smooth = T, which_fun = mean,
                                  return_plot = F) {

  assert_that(is.numeric(x), is.numeric(value), is.numeric(adjust),
              is.numeric(smoothfactor), is.numeric(groove_cutoff),
              is.logical(second_smooth), is.logical(return_plot),
              "function" %in% class(which_fun))

  land <- data.frame(x = x, value = value)
  original_land <- land

  if (!is.null(mean_left) && !is.null(mean_right)) {
    mean.left.ind <- which.min(abs(land$x - mean_left))
    mean.right.ind <- which.min(abs(land$x - mean_right))

    window.left.left <- max(1, mean.left.ind - mean_window)
    window.left.right <- mean.left.ind + mean_window

    window.right.left <- mean.right.ind - mean_window
    window.right.right <- min(length(land$x), mean.right.ind + mean_window)

    land <- land[c(window.left.left:window.left.right,
                       window.right.left:window.right.right), ]

    groove_cutoff <- Inf
  }

  value_filled <- na.fill(land$value, "extend")
  smoothed <- rollapply(value_filled, smoothfactor, function(x) which_fun(x))
  # Add in an if statement, to only do the first smoothing if the second_smooth parameter is equal to FALSE
  if (second_smooth == T) {
    smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) which_fun(x), partial = TRUE)
  }
  else {smoothed_truefalse <- smoothed}

  lengthdiff <- length(land$value) - length(smoothed_truefalse)

  peak_ind_smoothed <- head(which(rollapply(smoothed_truefalse, 3, function(x) which.max(x) == 2)), n = 1)
  peak_ind <- peak_ind_smoothed + floor(lengthdiff / 2)
  if (length(peak_ind) == 0) {
    groove_ind <- peak_ind
  } else {
    groove_ind <- head(which(rollapply(tail(smoothed_truefalse, n = -peak_ind_smoothed), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind
  }

  peak_ind2_smoothed_temp <- head(which(rollapply(rev(smoothed_truefalse), 3, function(x) which.max(x) == 2)), n = 1)
  peak_ind2_temp <- peak_ind2_smoothed_temp + floor(lengthdiff / 2)
  if (length(peak_ind2_temp) == 0) {
    groove_ind2_temp <- peak_ind2_temp
  } else {
    groove_ind2_temp <- head(which(rollapply(tail(rev(smoothed_truefalse), n = -peak_ind2_smoothed_temp), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind2_temp
  }

  # peak_ind2 <- length(land$value) - peak_ind2_temp + 1
  groove_ind2 <- length(land$value) - groove_ind2_temp + 1

  ## Check that it actually FOUND a groove...
  if (length(groove_ind) == 0 || groove_ind > groove_cutoff) groove_ind <- 1
  if (length(groove_ind2) == 0 || groove_ind2 < length(land$value) - groove_cutoff) groove_ind2 <- length(land$value)

  xvals <- original_land$x
  # yvals <- original_land$value

  # plot_peak_ind <- which(original_land$x == land$x[peak_ind])
  plot_groove_ind <- which(original_land$x == land$x[groove_ind])
  # plot_peak_ind2 <- which(original_land$x == land$x[peak_ind2])
  plot_groove_ind2 <- which(original_land$x == land$x[groove_ind2])

  center <- which.min(abs(xvals - mean(xvals)))

  if (plot_groove_ind > center) {
    plot_groove_ind2 <- plot_groove_ind
    plot_groove_ind <- 0
  }

  if (plot_groove_ind2 < center) {
    plot_groove_ind <- plot_groove_ind2
    plot_groove_ind2 <- length(xvals)
  }

  # smoothed_diff <- floor(lengthdiff/2)

  groove <- c(original_land$x[plot_groove_ind + adjust],
              original_land$x[plot_groove_ind2 - adjust])

  if (return_plot) {
    return(list(groove = groove,
                plot = grooves_plot(land = original_land, grooves = groove)))
  } else {
    return(list(groove = groove))
  }
}
