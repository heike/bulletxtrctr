# TODO: Get rid of br111 and other hard-to-understand variable names

#' Get a specified cross section
#'
#' @param y y coordinate of cross section
#' @param land x3p file with land data
#' @return cross cut data frame
#' @importFrom assertthat assert_that has_name
land_cc <- function(y, land) {
  value <- x <- NULL
  assert_that(has_name(land, "y"), has_name(land, "x"))

  # get cross cut, and smooth it
  ys <- unique(land$y)
  picky <- ys[which.min(abs(y - ys))]
  br111 <- land[land$y == picky, ]
  # inc <- bullet$header.info$incrementY
  ## XXX    br111.groove <- bulletr::get_grooves(br111, groove_cutoff = 400, smoothfactor = 15, adjust = 10)
  br111.groove <- quantile(br111$x, probs = c(0.15, 0.85))
  groove <- br111.groove
  br111_filter <- subset(br111, !is.na(value) & x > groove[1] & x < groove[2])
  dframe <- cc_fit_loess(br111_filter, span = 0.75)
  if (is.null(dframe$raw_sig)) browser()
  dframe$resid <- dframe$raw_sig # where are we still using resid?

  #   br111 <- switch_xy(br111)
  #   groove <- list(groove = br111.groove, plot = NULL)
  #   dframe <- bulletr::fit_loess(br111, groove)$resid$data
  #   dframe <- switch_xy(dframe)
  dframe
}

#' Check whether an x3p argument is character or filename, return an x3p object
#'
#' @param x3p if character, path to an x3p file. Otherwise a scan in x3p
#'          format is expected.
#' @importFrom x3ptools read_x3p
#' @importFrom assertthat assert_that
check_x3p <- function(x3p) {
  bullet <- NULL
  if (is.character(x3p)) bullet <- read_x3p(x3p)
  if ("x3p" %in% class(x3p)) bullet <- x3p
  stopifnot(!is.null(bullet))

  return(bullet)
}

#' Identify a reliable cross section
#'
#' Identifies a "representative" cross section for a bullet land engraved area.
#' Striation marks on a bullet land are best expressed at the heel (bottom) of
#' a bullet where break-off is still problematic.
#' Using cross-correlation we identify a cross section that is
#' the closest to the bottom of the bullet but does not suffer from break-off.
#' If the resulting cross section is equal to the maximum of the search area
#' (defined in xlimits), there should be some investigation to determine
#' whether this cross section is usable, due to the risk of tank rash.
#'
#' # TODO: are missing values only on the right hand side (leading shoulder)?
#' @param x3p if character, path to an x3p file. Otherwise a scan in x3p
#'          format is expected. The assumption is that the scan is taken across
#'          the bullet land, with an upright bullet, i.e.  heel along x with
#'          y = 0. (0,0) defines the bottom left corner of the scan.
#' @param distance positive numeric value indicating the distance between cross
#'          sections to use for a comparison
#' @param ylimits vector of values between which to check for cross sections in
#'          a stable region. In case the upper limit is not specified
#'          explicitly, it is determined by the scan itself.
#' @param minccf minimal value of cross correlation to indicate a stable region
#' @param span The span for the loess smooth function
#' @param percent_missing maximum percent missing values allowed on the crosscut
#'          to be picked
#' @return dataframe of crosscut
#' @importFrom x3ptools x3p_to_df
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
#'   stop_at_step = "clean",
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' x3p_crosscut_optimize(example_data$x3p[[1]])
#' x3p_crosscut(example_data$x3p[[1]], 75) %>%
#'   ggplot(aes(x = x, y = value)) + geom_line()
#' }
x3p_crosscut_optimize <- function(x3p, distance = 25, ylimits = c(50, NA),
                                  minccf = 0.9, span = 0.03,
                                  percent_missing = 50) {
  bullet <- check_x3p(x3p)

  dbr111 <- x3ptools::x3p_to_df(bullet)
  if (is.na(ylimits[2])) {
    ylimits[2] <- max(dbr111$y)
  }

  done <- FALSE
  y <- min(ylimits)
  first_cc <- land_cc(y, land = dbr111)

  # This loop only entered when there is too much missingness - too hard to test
  while ((dim(first_cc)[1] < bullet$header.info$sizeX * percent_missing / 100) &
         (y < bullet$header.info$sizeY)) {
    y <- y + distance
    first_cc <- land_cc(y, land = dbr111)
  }

  while (!done) {
    y <- y + distance
    # TODO: need to check that we have enough data
    second_cc <- land_cc(y, land = dbr111)
    #    res <- ccf(first_cc$resid, second_cc$resid, lag.max = .5*min(nrow(first_cc), nrow(second_cc)), plot=FALSE)
    #    ccf <- max(res$acf)

    first_cc$bullet <- "first-bullet"
    second_cc$bullet <- "second-bullet"

    # smooth raw signatures, then align and compare:
    first_cc$l30 <- raw_sig_smooth(first_cc$raw_sig, span = span)
    second_cc$l30 <- raw_sig_smooth(second_cc$raw_sig, span = span)
    ccf <- sig_align(first_cc$l30, second_cc$l30)$ccf

    if (ccf > minccf) {
      done <- TRUE
      return(y - distance)
    }
    first_cc <- second_cc
    if (y + distance > max(ylimits)) done <- TRUE
  }
  return(NA)
}

#' Switch x and y in a data frame
#'
#' Used to relable x3ps
#' @param dataframe a data frame with columns x and y
#' @return dataframe
#' @importFrom assertthat assert_that has_name
switch_xy <- function(dataframe) {
  assert_that(is.data.frame(dataframe),
              has_name(dataframe, "x"),
              has_name(dataframe, "y"))
  # switch x and y
  xidx <- grep("x", names(dataframe))
  yidx <- grep("y", names(dataframe))
  names(dataframe)[c(yidx, xidx)] <- c("x", "y")
  dataframe
}


#' Read a crosscut from a 3d surface file
#'
#' @param x3p  if character, path to an x3p file. Otherwise a scan in x3p
#'          format is expected.
#' @param y level of the crosscut to be taken. If this level does not exist,
#'          the crosscut along the middle of the land is returned.
#' @return data frame
#' @importFrom x3ptools read_x3p
#' @importFrom x3ptools x3p_to_df
#' @importFrom zoo na.trim
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
#'   stop_at_step = "clean",
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' x3p_crosscut_optimize(example_data$x3p[[1]])
#' x3p_crosscut(example_data$x3p[[1]], 75) %>%
#'   ggplot(aes(x = x, y = value)) + geom_line()
#' }
x3p_crosscut <- function(x3p, y = NULL) {
  bullet <- check_x3p(x3p)

  # TODO: check into how na.trim is used here
  dbr111 <- na.trim(x3p_to_df(bullet))
  ys <- unique(dbr111$y)
  if (is.null(y)) y <- median(ys)

  picky <- ys[which.min(abs(y - ys))]
  dbr111.fixx <- dbr111[dbr111$y == picky, ]

  return(na.omit(dbr111.fixx))
}

#' Check object returned by x3p_crosscut_optimize
#'
#' @param x data frame from x3p_crosscut_optimize
#' @return TRUE if everything is ok, error otherwise
#' @importFrom assertthat assert_that has_name
check_ccdata <- function(x) {
  assert_that(!is.null(x), msg = "crosscut data must not be null")
  assert_that(has_name(x, "x"), has_name(x, "value"))
  assert_that(nrow(x) > 0, msg = "crosscut data must have > 0 rows")
}
