#' Identify the number of maximum CMS between two signatures
#'
#' adapted from `bulletGetMaxCMS`
#' aligns two signatures, identifies peaks and valleys, matches striae, and
#' counts longest run.
#' @param aligned data frame of location and aligned signatures
#' @param span positive number  for the smoothfactor to use for assessing peaks.
#' @return list of matching parameters, data set of the identified striae,
#'    and the aligned data sets.
#' @export
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(
#'     Bullet1 = c(hamby252demo$bullet1[2]),
#'     Bullet2 = c(hamby252demo$bullet1[4])
#'   ),
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' alignment <- sig_align(example_data$sigs[[1]]$sig,
#'                        example_data$sigs[[2]]$sig)
#' striae <- sig_cms_max(alignment)
#' }
sig_cms_max <- function(aligned, span = 35) {
  check_align(aligned)

  sigX <- aligned$bullets

  peaks1 <- sig_get_peaks(sigX$sig1, smoothfactor = span)
  peaks2 <- sig_get_peaks(sigX$sig2, smoothfactor = span)

  assert_that(has_name(peaks1, "lines"), has_name(peaks2, "lines"))

  peaks1$lines$bullet <- "sig1"
  peaks2$lines$bullet <- "sig2"

  lines <- striation_identify_matches(peaks1$lines, peaks2$lines)

  maxCMS <- get_longest_run(lines$match == TRUE)
  list(maxCMS = maxCMS, ccf = aligned$ccf, lag = aligned$lag,
       lines = lines, bullets = sigX)
}

#' Length of the longest run of TRUEs
#'
#' Identifies the length of the longest run of TRUEs in Boolean vector `x`.
#' used to be `maxCMS`
#' @param x  Boolean vector
#' @return an integer value of the length of the longest run of TRUE values
#' @export
#' @examples
#' x <- rbinom(100, size = 1, prob = 1 / 3)
#' get_runs(x == 1) # expected value for longest match is 3
#' get_longest_run(x == 1)
get_longest_run <- function(x) {
  runTable <- get_runs(x)

  assert_that(
    is.numeric(runTable),
    !is.null(dimnames(runTable))
  )

  as.numeric(rev(names(runTable)))[1]
}

#' Table of the number of runs
#'
#' Identify the length of runs (of values TRUE) and their frequencies.
#' @param x Boolean vector
#' @return a table of the number of runs of TRUEs
#' @export
#' @examples
#' x <- rbinom(100, size = 1, prob = 1 / 3)
#' get_runs(x == 1) # expected value for longest run is 3
#' get_runs(x == 0) # expected value for longest run is 6
get_runs <- function(x) {
  assert_that(is.numeric(x) | is.logical(x))

  # number of runs of different lengths
  if (!is.logical(x)) {
    warning("Converting x to a logical vector")
    x <- as.logical(x)
  }

  y <- diff(x)
  # y is -1 if change from 1 to 0,
  #       0 if unchanged
  #       1 if change from 0 to 1
  w <- c(0, y)[x][-1]

  z <- which(w == 1)
  z <- c(0, z, length(x[x]))

  return(table(diff(z)))
}
