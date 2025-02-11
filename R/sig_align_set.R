#' Get lags between signatures
#'
#' Extract lags for aligning all signatures in a set with respect to the signature specified in 'align_to'.
#' Alignments will vary depending on which signature is used for alignment.
#' @param data data frame with ids of signatures, x variable and `value` variable.
#' @param value symbol for the signature values
#' @param sig_id factor variable with signatures ids
#' @param min.overlap value passed on to `?get_ccf`.
#' @param align_to id of the signature used for alignment. Defaults to the first factor level.
#' @param resolution numeric value representing the resolution of the signature in microns per pixel.
#' @param tol tolerance used, i.e. smallest value, such that 1 + tol not equal to 1.
#' Can't be smaller than the machine's numeric resolution `.Machine$double.eps`.
#' @return data frame with a summary (one row for each level of `sig_id`) containing
#' the information on `id`, `lag`, and `ccf`.
#' @export
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' @importFrom dplyr group_by mutate select pull lag
#' @importFrom utils head
#' @examples
#' \dontrun{
#' tmaRks::data("toolmarks", package="tmaRks")
#' tnest <- toolmarks %>% filter(size=="L") %>% group_by(side, angle) %>% tidyr::nest()
#' tnest <- tnest %>% mutate(
#'   lags = data %>% purrr::map(.f = get_sig_lags, value=signature,
#'               sig_id=TID, min.overlap=400, resolution = 7.0505e-03)
#' )
#' }
get_sig_lags <- function (data, value, sig_id, min.overlap=500,
                          align_to=levels(sig_id)[1], resolution=0.645, tol=1e-06) {

  id_name <- quo_name(enquo(sig_id))
  value_name <- quo_name(enquo(value))

  stopifnot(all("x" %in% names(data), id_name %in% names(data), value_name %in% names(data)))
  # make sure that x values are on an equidistant grid
  diffx <- data$x %>% unique() %>% sort() %>% diff()

  if (!all(near(diffx[1], diffx, tol = tol))) {
  #  browser()
    stop(sprintf("Resolution in x not equidistant: %s, ...", resolution, paste(head(diffx), collapse=",")))
  }
  if (!all(near(resolution, diffx[1],tol = tol))) stop(sprintf("Resolution in x (%f) not equal to resolution specified in function call (%f)", diffx[1], resolution))


  # if("x" %in% names(data)) {
  #   diffx <- data$x %>% unique() %>% sort() %>% diff()
  #   cat("For a horizontal crosscut we only have one value here.")
  # }
  dlist <- data %>% group_by({{ sig_id }}) %>% tidyr::nest()
  dlist <- dlist %>% mutate(data = data %>% purrr::map(.f = function(d) {
    # align all signatures to the first
    aligned <- sig_align(dlist$data[[1]] %>%
      select({{ value }}) %>% pull, d %>% select({{ value }}) %>%
        pull, min.overlap = min.overlap)
    idx1 <- which(!is.na(aligned$lands$sig1))[1]
    idx2 <- which(!is.na(aligned$lands$sig2))[1]

    # we assume that d is ordered in x
    d %>% mutate(
      aligned_x = (1:nrow(d) - idx1 + idx2)*resolution,
      # sig_align reports back index - need to get it back into the right resolution
      ccf = aligned$ccf,
      lag = idx2 - idx1) #
  }))

  # now unnest to the longform again
  long <- dlist %>%  tidyr::unnest(cols = data)
  long
}


