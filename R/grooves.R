#' Find the grooves of a bullet land
#'
#' @param ccdata data frame of the crosscut. Data frame needs location x and
#'          measured values as `value`. If multiple crosscuts are to be
#'          considered, include a variable y and use as a key.
#' @param method method to use for identifying grooves. One of "quadratic", "rollapply", "middle", "logisticlegacy", "lassobasic", "lassofull", "bcp". Defaults to "rollapply"
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
#' @import grooveFinder
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
#'       x3p_scale_unit(scale_by=10^6) %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "rollapply",
#'   adjust = 30, return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "middle",
#'   adjust = 30, return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "quadratic",
#'   adjust = 30, return_plot = T
#' )
#' }
cc_locate_grooves <- function(ccdata, method = "rollapply", smoothfactor = 15,
                              adjust = 10, groove_cutoff = 400,
                              mean_left = NULL, mean_right = NULL,
                              mean_window = 100,
                              return_plot = F, ...) {
  # TODO add documentation for different groove options, either here or by exporting the individual functions and documenting the parameters there.

  x <- y <- value <- NULL
  land <- ccdata
  if (is.null(ccdata) || nrow(ccdata) == 0) return(NULL)

  check_ccdata(ccdata)

  assert_that(method %in% c("quadratic", "rollapply", "middle", "logisticlegacy",
                            "lassobasic", "lassofull", "bcp", "hough")) ## too strict

  # TODO: expand cc_locate_groove to accept user defined get_grooves_XXX function
  assert_that(
    is.numeric(smoothfactor), is.numeric(adjust), is.numeric(groove_cutoff),
    is.logical(return_plot)
  )

  if (method == "logisticlegacy") {
    grooves <- get_grooves_logisticlegacy(
      x = land$x, value = land$value,
      adjust = adjust, return_plot = return_plot
    )
  }
  if (method == "lassobasic") {
    grooves <- get_grooves_lasso(
      x = land$x, value = land$value, lasso_method = "basic",
      return_plot = return_plot, ...
    )
  }
  if (method == "lassofull") {
    grooves <- get_grooves_lasso(
      x = land$x, value = land$value, lasso_method = "full",
      return_plot = return_plot, ...
    )
  }
  if (method == "quadratic") {
    # grooves <- get_grooves_quadratic(bullet = bullet, adjust = adjust)
    grooves <- get_grooves_quadratic(
      x = land$x, value = land$value,
      adjust = adjust, return_plot = return_plot
    )
  }
  if (method == "rollapply") {
    # make sure there is only one x
    if (length(unique(land$y)) > 1) {
      message(sprintf(
        "summarizing %d profiles by averaging across values\n",
        length(unique(land$y))
      ))
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
      return_plot = return_plot,
      ...
    )
  }
  if (method == "middle") {
    middle <- 75
    if ("middle" %in% names(list(...))) {
      middle <- list(...)$middle
    }
    grooves <- get_grooves_middle(
      x = land$x, value = land$value,
      middle = middle,
      return_plot = return_plot
    )
  }

  if (method == "bcp") {
    grooves <- grooveFinder::get_grooves_bcp(
      x = land$x, value = land$value,
      adjust = adjust,
      return_plot = return_plot
    )
  }

  if (method == "hough") {
    norm.index <- list(...)$norm.index
    if (is.null(norm.index)) norm.index <- 1
    grooves <- get_grooves_hough(land = land, norm.index = norm.index, adjust = adjust,
                                 return_plot = return_plot)
  }

  return(grooves)
}





#' Check grooves for correctness
#'
#' @param x output from cc_locate_grooves
#' @return TRUE if ok, error otherwise
#' @importFrom assertthat assert_that
check_grooves <- function(x) {
  assert_that(has_name(x, "groove"))
  assert_that(is.numeric(x$groove))
  assert_that(length(x$groove) <= 2)
  return(TRUE)
}
