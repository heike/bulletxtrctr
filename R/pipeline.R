#' Helper file to setup data
#'
#' @param directory directory containing x3p files. If there are sub-directories,
#'          this function will assume that each sub-directory contains lands
#'          from different bullets and will separate them accordingly
#' @param stop_at_step One of read, clean, crosscut, grooves, signatures
#' @param x3p_clean function to use to clean the x3p file - convert header info
#'          to correct units, rotate the surface matrix, etc.
#' @param ... additional arguments to cc_locate_grooves and cc_get_signature
bullet_pipeline <- function(
  directory, stop_at_step = NULL, x3p_clean = function(x) x, ...) {

  if (is.null(stop_at_step)) stop_at_step <- "signatures"

  assert_that(dir.exists(directory))
  assert_that("function" %in% class(x3p_clean))
  assert_that(stop_at_step %in% c("read", "clean", "crosscut", "grooves", "signatures"))

  dots <- list(...)

  dirfiles <- list.files(directory, pattern = "x3p", full.names = T, recursive = T)
  dirs <- dirname(dirfiles) %>% unique()

  assert_that(length(dirfiles) > 0)

  land_list <- read_bullet(dirs) %>%
    # I don't know if this will work on Windows...
    dplyr::mutate(bullet = sub(pattern = "(.*)/(.*?)$", replacement = "\\2",
                               dirname(as.character(source)))) %>%
    select(source, bullet, x3p)

  assert_that(has_name(land_list, "source"),
              has_name(land_list, "bullet"),
              has_name(land_list, "x3p"))

  if (stop_at_step == "read") return(land_list)

  land_list <- land_list %>%
    mutate(x3p = purrr::map(x3p, .f = x3p_clean))

  if (stop_at_step == "clean") return(land_list)

  ccnames <- names(formals(x3p_crosscut_optimize))
  ccargs <- dots[names(dots) %in% ccnames]

  land_list <- land_list %>%
    mutate(
      cclist = lapply(x3p, function(x) {
        ccargs$x3p <- x
        return(ccargs)
      }),
      crosscut = purrr::map_dbl(
        .x = cclist,
        .f = ~do.call("x3p_crosscut_optimize", .x)
      )
    ) %>%
    mutate(ccdata = purrr::map2(x3p, crosscut, x3p_crosscut)) %>%
    select(-cclist)

  assert_that(has_name(land_list, "crosscut"),
              has_name(land_list, "ccdata"))

  if (stop_at_step == "crosscut") return(land_list)

  gnames <- names(formals(cc_locate_grooves))
  gargs <- dots[names(dots) %in% gnames]

  land_list <- land_list %>% mutate(
    glist = lapply(ccdata, function(x) {
      gargs$ccdata <- x
      return(gargs)
    }),
    grooves = purrr::map(
      .x = glist,
      .f = ~do.call("cc_locate_grooves", .x))
  ) %>%
    select(-glist)

  assert_that(has_name(land_list, "grooves"))

  if (stop_at_step == "grooves") return(land_list)

  snames <- names(formals(cc_get_signature))
  sargs <- dots[names(dots) %in% snames]

  land_list <- land_list %>% mutate(
    slist = purrr::map2(ccdata, grooves, function(x, y) {

      sargs$ccdata <- x
      sargs$grooves <- y
      return(sargs)
    }),
    sigs = purrr::map(
      .x = slist,
      .f = ~do.call("cc_get_signature", .x))
  ) %>%
    select(-slist)

  assert_that(has_name(land_list, "sigs"))

  return(land_list)

}

#' Convert x3p header information to microns from meters
#'
#' @param x3p x3p data read in using read_x3p or read_bullet
#' @return x3p with header information in microns
#' @export
x3pheader_to_microns <- function(x3p) {
    # make sure all measurements are in microns
    x3p$surface.matrix <- x3p$surface.matrix*10^6
    x3p$header.info$incrementY <- x3p$header.info$incrementY*10^6
    x3p$header.info$incrementX <- x3p$header.info$incrementX*10^6
    x3p
}
