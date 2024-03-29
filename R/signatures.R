#' Extract signature from crosscut
#'
#' x3p file of a 3d topological bullet surface is processed at surface
#' crosscut y,
#' measurements outside the bullet grooves in the crosscuts are left out, and
#' a loess smooth is used (see \code{?loess} for details) to remove the big
#' structure.
#' @param ccdata crosscut as returned from x3p_crosscut
#' @param grooves The grooves to use as a two element vector, if desired
#' @param span1 The span for the loess fit to get from the profile to the raw
#'          signature
#' @param span2 The span for the loess fit to smooth the raw signature; if `span2` is set to NA, no second smoothing will be done.
#' @return data frame
#' @import dplyr
#' @export
#' @examples
#' data(br411)
#' cc <- x3p_crosscut_optimize(br411)
#' ccdata <- x3p_crosscut(br411, cc)
#' grooves <- cc_locate_grooves(ccdata)
#' signature <- cc_get_signature(ccdata, grooves)
#'
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   x3p_clean = function(x) x %>%
#'       x3p_scale_unit(scale_by=10^6) %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' cc_get_signature(example_data$ccdata[[1]],
#'                  example_data$grooves[[1]]) %>%
#' head()
#' }
cc_get_signature <- function(ccdata, grooves=NULL, span1 = 0.75, span2 = 0.03) {
  x <- y <- value <- raw_sig <- se <- NULL
  check_ccdata(ccdata)

  if(is.null(grooves)) {
    grooves <- list(groove=range(ccdata$x))
  }

  check_grooves(grooves)

  if (is.null(ccdata)  | nrow(ccdata) == 0) return(NULL)

  # TODO: Clean this up and use proper names for things
  br111 <- na.trim(ccdata) %>%
    filter(between(x, grooves$groove[1], grooves$groove[2])) %>%
    group_by(x) %>%
    summarise(
      y = mean(y),
      value = mean(value, na.rm = TRUE)
    ) %>%
    dplyr::select(x, y, value) %>%
    as.data.frame()

  myspan <- span1
  if (myspan > 1) {
    ## Use the nist method
    myspan <- myspan / diff(grooves$groove)
  }
  loess_model <- cc_fit_loess(br111, span = myspan)
  check_loess_fit(loess_model)


  ccdata <- ccdata %>%
    left_join(loess_model %>% dplyr::select(x, raw_sig, se), by = "x")

  if (is.na(span2)) {
    ccdata$sig <- ccdata$sig_raw
  } else {
  myspan <- ifelse(span2 > 1, span2 / diff(grooves$groove), span2)
  ccdata$sig <- with(ccdata,
    predict(loess(raw_sig ~ x, span = myspan), newdata = ccdata
  ))
  }

  ccdata
}

#' Check signature object
#'
#' @param x output from cc_get_signature
#' @return TRUE or error
#' @importFrom assertthat assert_that has_name
check_sig <- function(x) {
  assert_that(has_name(x, "sig"), has_name(x, "raw_sig"))
}
