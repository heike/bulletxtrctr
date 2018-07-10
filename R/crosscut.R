#' Get a specified cross section
#'
#' @param y y coordinate of cross section
#' @param land x3p file with land data
#' @return cross cut data frame
land_cc <- function(y, land) {
  # get cross cut, and smooth it
  ys <- unique(land$y)
  picky <- ys[which.min(abs(y - ys))]
  br111 <- land[land$y == picky,]
  #inc <- bullet$header.info$incrementY
  ## XXX    br111.groove <- bulletr::get_grooves(br111, groove_cutoff = 400, smoothfactor = 15, adjust = 10)
  br111.groove <- quantile(br111$x, probs = c(0.15, 0.85))
  #    br111.groove$plot
  #    browser()

  br111 <- switch_xy(br111)
  groove <- list(groove = br111.groove, plot = NULL)
  dframe <- bulletr::fit_loess(br111, groove)$resid$data
  dframe <- switch_xy(dframe)
  ### XXX need to get fit_loess out of this

  #    path <- gsub(".*//", "", as.character(path))
  #    dframe$bullet <- paste(gsub(".x3p", "", path), x)
  dframe
}

#' Identify a reliable cross section
#'
#' Identifies a "representative" cross section for a bullet land engraved area.
#' Striation marks on a bullet land are the
#' best expressed at the heel (bottom) of a bullet where break-off is still problematic.
#' Using cross-correlation we identify a cross section that is
#' the closest to the bottom of the bullet but does not suffer from break-off.
#' If the resulting cross section is equal to the maximum of the search area (defined in xlimits),
#' there should be some investigation, whether this cross section is usable.
#' There is the risk of tank rash.
#'
#' XXX still to do: are missing values only on the right hand side (leading shoulder)?
#' @param x3p  if character, path to an x3p file. Otherwise a scan in x3p format is expected.
#' The assumption is that the scan is taken across the bullet land, with an upright bullet, i.e.  heel along x with y = 0. (0,0) defines the bottom left corner of the scan.
#' @param distance positive numeric value indicating the distance between cross sections to use for a comparison
#' @param ylimits vector of values between which to check for cross sections in a stable region. In case the upper limit is not specified explicitly, it is determined by the scan itself.
#' @param minccf minimal value of cross correlation to indicate a stable region
#' @param span The span for the loess smooth function
#' @param percent_missing maximum percent missing values allowed on the crosscut to be picked
#' @return dataframe of crosscut
#' @importFrom x3ptools read_x3p
#' @importFrom x3ptools x3p_to_df
#' @export
x3p_crosscut_optimize <- function(x3p, distance = 25, ylimits = c(50, NA), minccf = 0.9, span = 0.03, percent_missing = 50) {

  bullet <- NULL
  if (is.character(x3p)) bullet <- read_x3p(x3p)
  if ("x3p" %in% class(x3p)) bullet <- x3p
  stopifnot(!is.null(bullet))

  dbr111 <- x3p_to_df(bullet)
  if (is.na(ylimits[2])) ylimits[2] <- max(dbr111$y)
  done <- FALSE
  y <- min(ylimits)
  first_cc <- land_cc(y, land = dbr111)

  while((dim(first_cc)[1] < bullet$header.info$sizeX*percent_missing/100) & (y < bullet$header.info$sizeY)) {
    y <- y + distance
    first_cc <- land_cc(x, land = dbr111)
  }

  while(!done) {
    y <- y + distance
    second_cc <- land_cc(y, land = dbr111) # need to check that we have enough data
#    res <- ccf(first_cc$resid, second_cc$resid, lag.max = .5*min(nrow(first_cc), nrow(second_cc)), plot=FALSE)
#    ccf <- max(res$acf)

    first_cc$bullet <- "first-bullet"
    second_cc$bullet <- "second-bullet"
    b2 <- rbind(first_cc, second_cc)
#browser()
    b2 <- switch_xy(b2)
      lofX <- bulletr::bulletSmooth(b2, span = span)
      ccf <- bulletr::bulletAlign(lofX)$ccf
    # would need to switch b2 back if it were to be used again

    if (ccf > minccf) {
      done <- TRUE
      return (y - distance)
    }
    first_cc <- second_cc
    if (y + distance > max(ylimits)) done <- TRUE
  }
  return (NA)
}

switch_xy <- function(dataframe) {
  # switch x and y
  xidx <- grep("x", names(dataframe))
  yidx <- grep("y", names(dataframe))
  names(dataframe)[c(yidx, xidx)] <- c("x", "y")
  dataframe
}


#' Read a crosscut from a 3d surface file
#'
#' @param x3p  if character, path to an x3p file. Otherwise a scan in x3p format is expected.
#' @param y level of the crosscut to be taken. If this level does not exist, the  crosscut along the middle of the land is returned.
#' @return data frame
#' @importFrom x3ptools read_x3p
#' @importFrom x3ptools x3p_to_df
#' @importFrom zoo na.trim
#' @export
x3p_crosscut <- function(x3p, y = NULL) {

  bullet <- NULL
  if (is.character(x3p)) bullet <- read_x3p(x3p)
  if ("x3p" %in% class(x3p)) bullet <- x3p
  stopifnot(!is.null(bullet))


  dbr111 <- na.trim(x3p_to_df(bullet)) # XXXX na.trim - check into how this is used here
  ys <- unique(dbr111$y)
  if (is.null(y)) y <- median(ys)

  picky <- ys[which.min(abs(y - ys))]
  dbr111.fixx <- dbr111[dbr111$y == picky,]

  return(dbr111.fixx)
}
