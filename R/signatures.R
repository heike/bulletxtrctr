#' Extract signature from crosscut
#'
#' x3p file of a 3d topological bullet surface is processed at surface
#' crosscut y,
#' measurements outside the bullet grooves in the crosscuts are left out, and
#' a loess smooth
#' is used (see \code{?loess} for details) to remove the big structure.
#' @param ccdata crosscut as returned from x3p_crosscut
#' @param grooves The grooves to use as a two element vector, if desired
#' @param span1 The span for the loess fit to get from the profile to the raw signature
#' @param span2 The span for the loess fit to smooth the raw signature
#' @return data frame
#' @import dplyr
#' @export
#' @examples
#' data(br411)
#' br411_processed <- processBullets(br411, name = "br411")
cc_get_signature <- function(ccdata, grooves = NULL, span1 = 0.75, span2 = 0.03) {

    br111 <- na.trim(ccdata) %>%
      filter(between(x, grooves$groove[1], grooves$groove[2])) %>%
      group_by(x) %>%
      summarise(y = mean(y),
                value = mean(value, na.rm = TRUE)) %>%
      dplyr::select(x, y, value) %>%
      as.data.frame

    myspan <- span1
    if (myspan > 1) {
      ## Use the nist method
      myspan <- myspan / diff(grooves$groove)
    }
    loess_model <- cc_fit_loess(br111, span = myspan)


    ccdata <- ccdata %>% left_join(loess_model %>% dplyr::select(x, raw_sig, se), by="x")

    myspan = ifelse(span2 > 1, span2 / diff(grooves$groove), span2)
    ccdata$sig = with(ccdata, predict(loess(raw_sig~x, span = myspan), newdata = ccdata))

    ccdata
}

#' Smooth the raw signature to remove excess variability
#'
#' @param data data frame as returned by the function \code{cc_get_raw_signature}
#' @param span width of the smoother, defaults to 0.03
#' @param limits vector of the form c(min, max). Results will be limited to be between these values.
#' @param id variable name of the land identifier
#' @return data frame of the same form as the input extended by the vector sig for the smoothed signature.
#' @importFrom dplyr mutate
#' @export
cc_get_signature_notused <- function(data, span = 0.03, limits = c(-5,5), id="bullet") {
  bullet <- NULL
  y <- NULL
  myspan <- NULL

  lof <- data %>% group_by_(id) %>% mutate(
    myspan = ifelse(span > 1, span / diff(range(y)), span),
    l30 = smoothloess(y, resid, span = myspan[1])
  ) %>% select(-myspan)
  lof$l30 <- pmin(max(limits), lof$l30)
  lof$l30 <- pmax(min(limits), lof$l30)
  lof
}

