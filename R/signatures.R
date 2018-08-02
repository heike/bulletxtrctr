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
  x <- y <- value <- raw_sig <- se <- NULL

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

