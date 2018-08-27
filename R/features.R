#' Extract number of consecutively matching elevated striation marks from the
#' right of two aligned signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return number of consecutively matching striation marks (from right)
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr '%>%' arrange
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
#' extract_feature_right_cms(striae$lines)
#' }
extract_feature_right_cms <- function(striae) {
  assert_that(
    has_name(striae, "xmin"),
    has_name(striae, "match")
  )
  xmin <- NULL

  striae <- striae %>% arrange(xmin)
  striae$rightcms <- FALSE
  idx <- which(striae$match == FALSE)
  lastidx <- length(idx)
  if (lastidx > 0) {
    return(nrow(striae) - idx[lastidx])
  }
  return(nrow(striae))
}

#' Extract number of consecutively matching elevated striation marks from the
#' left of two aligned signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return number of consecutively matching striation marks (from left)
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr '%>%' arrange
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
#' extract_feature_left_cms(striae$lines)
#' }
extract_feature_left_cms <- function(striae) {
  assert_that(
    has_name(striae, "xmin"),
    has_name(striae, "match")
  )
  xmin <- NULL

  striae <- striae %>% arrange(xmin)
  striae$leftcms <- FALSE
  idx <- which(striae$match == FALSE)
  if (length(idx) > 0) {
    return(idx[1] - 1)
  }

  return(nrow(striae))
}

#' Extract number of consecutively matching elevated striation marks from two
#' aligned signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return number of consecutively matching elevated striation marks
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr filter arrange '%>%'
extract_feature_cms2 <- function(striae) {
  assert_that(
    has_name(striae, "xmin"),
    has_name(striae, "type"),
    has_name(striae, "match")
  )
  xmin <- type <- NULL

  striae <- striae %>% arrange(xmin)
  peaks <- filter(striae, type == 1)
  get_longest_run(peaks$match == TRUE)
}

#' Extract number of consecutively matching striation marks from two aligned
#' signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return number of consecutively matching striation marks
#' @export
#' @importFrom assertthat assert_that has_name
extract_feature_cms <- function(striae) {
  assert_that(has_name(striae, "match"))
  get_longest_run(striae$match == TRUE)
}


#' Extract number of consecutively non-matching striation marks from two
#' aligned signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return number of consecutively non-matching striation marks
#' @export
#' @importFrom assertthat assert_that has_name
extract_feature_non_cms <- function(striae) {
  assert_that(has_name(striae, "match"))
  get_longest_run(striae$match == FALSE)
}

#' Extract information for striation marks from two aligned signatures
#'
#' internal function, called by multiple extract_feature functions
#' @param striae data frames of striation marks based on two aligned signatures
#' @param type one of "peak", "valley" or "all"
#' @param match binary setting: TRUE for matching striae, FALSE for non-matching
#'          striae
#' @return number of striae
#' @importFrom assertthat assert_that has_name
extract_helper_feature_n_striae <- function(striae, type = "peak", match = TRUE) {
  assert_that(
    has_name(striae, "type"),
    has_name(striae, "match")
  )
  assert_that(type %in% c("peak", "valley", "all"),
    msg = "type must be peak, valley, or all"
  )

  striae$type__ <- TRUE

  if (type == "peak") striae$type__ <- striae$type == 1
  if (type == "valley") striae$type__ <- striae$type == -1

  if (match) {
    n <- sum(striae$match & striae$type__)
  } else {
    n <- sum(!striae$match)
  }

  return(n)
}

#' Extract number of matching striation marks from two aligned signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return number of matching striation marks
#' @export
extract_feature_matches <- function(striae) {
  extract_helper_feature_n_striae(striae, type = "all", match = TRUE)
}

#' Extract number of mismatched striation marks from two aligned signatures
#'
#' @param striae data frames of striation marks based on two aligned signatures
#' @return number of mismatched striation marks
#' @export
extract_feature_mismatches <- function(striae) {
  extract_helper_feature_n_striae(striae, type = "all", match = FALSE)
}

#' Extract the combined height of aligned striae between two aligned signatures
#'
#' @param striae data frame of striation marks based on two aligned signatures
#' @return sum of heights of matching striae
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr filter summarize '%>%'
extract_feature_sum_peaks <- function(striae) {
  heights <- match <- NULL
  assert_that(
    has_name(striae, "heights"),
    has_name(striae, "match")
  )
  striae %>%
    filter(match == TRUE) %>%
    summarize(
      sum_peaks = sum(abs(heights), na.rm = TRUE)
    ) %>%
    as.numeric()
}

#' Extract ccf from two (or more) aligned signatures
#'
#' @param aligned data frame with variable x (for location) and two or
#'          more measurements
#' @return (matrix) of correlations
#' @importFrom stats cor
#' @importFrom assertthat assert_that
#' @export
extract_feature_ccf <- function(aligned) {
  assert_that(dim(aligned)[2] > 2, msg = "aligned must have at least 3 columns")
  for (i in 2:dim(aligned)[2]) {
    assert_that(
      is.numeric(aligned[, i]),
      msg = sprintf("Column %d (%s) is not numeric", i, names(aligned)[i]))
  }

  if (dim(aligned)[2] == 3) { # only two signatures to compare
    return(cor(aligned[, 2], aligned[, 3], use = "pairwise"))
  }
  return(cor(aligned[, -1], use = "pairwise"))
}

#' Extract lag from two (or more) aligned signatures
#'
#' In the case of two signatures the result is an integer definining the number
#' of indices by which the second signature is shifted compared to the first
#' signature in the alignment. Note that this lag can be negative.
#' In the case of multiple signatures the result is a vector of non-negative
#' integers of the same length as signatures. Each element gives the number of
#' indices by which the corresponding signature is shifted compared to the
#' *first* signature. By definition, one of the numbers has to be 0 indicating
#' the *first* signature. XXX Need to indicate that "first" isn't the col index
#'
#' @param aligned data frame with variable x (for location) and two or
#'          more measurements
#' @return (vector) of lags
#' @importFrom assertthat assert_that
#' @export
extract_feature_lag <- function(aligned) {
  assert_that(dim(aligned)[2] > 2, msg = "aligned must have at least 3 columns")
  for (i in 2:dim(aligned)[2]) {
    assert_that(
      is.numeric(aligned[, i]),
      msg = sprintf("Column %d (%s) is not numeric", i, names(aligned)[i]))
  }

  lags <- sapply(aligned[, -1], function(x) {
    if (!is.na(x[1])) return(0)
    diffs <- diff(is.na(x))
    which(diffs == -1)
  })

  if (length(lags) == 2) return(diff(lags))
  lags
}

#' Extract average distance between two (or more) aligned signatures
#'
#' @param aligned data frame with variable x (for location) and two or more
#'          measurements
#' @param ... arguments for function `dist`
#' @return object of class distance
#' @importFrom stats dist as.dist
#' @importFrom assertthat assert_that
#' @export
extract_feature_D <- function(aligned, ...) {
  assert_that(dim(aligned)[2] > 2, msg = "aligned must have at least 3 columns")
  for (i in 2:dim(aligned)[2]) {
    assert_that(
      is.numeric(aligned[, i]),
      msg = sprintf("Column %d (%s) is not numeric", i, names(aligned)[i]))
  }

  dists <- dist(t(aligned[, -1]), ...)
  ns <- t(!is.na(aligned[, -1])) %*% !is.na(aligned[, -1])

  if (attr(dists, "Size") == 2) return(as.numeric(dists / as.dist(ns)))

  dists / as.dist(ns)
}

#' Extract length of two (aligned) signatures
#'
#' Signatures will usually be of different lengths. In a comparison, the length
#' of the shorter signature represents the potential length for a match.
#' @param aligned data frame with variable x (for location) and two or more
#'          measurements
#' @return integer value of the length of the shorter signature.
#' @importFrom assertthat assert_that
#' @export
extract_feature_length <- function(aligned) {
  assert_that(dim(aligned)[2] > 2, msg = "aligned must have at least 3 columns")
  for (i in 2:dim(aligned)[2]) {
    assert_that(
      is.numeric(aligned[, i]),
      msg = sprintf("Column %d (%s) is not numeric", i, names(aligned)[i]))
  }

  # only smaller of the length of the first two signatures
  n1 <- sum(!is.na(aligned[, 2]))
  n2 <- sum(!is.na(aligned[, 3]))

  min(n1, n2)
}

#' Extract overlap between two aligned signatures
#'
#' The overlap of two aligned signatures is defined as the ratio of the number
#' of non-missing overlapping values of the two aligned signatures and
#' the length of the shorter signature. A larger overlap indicates a higher
#' level of agreement between the signatures.
#' @param aligned data frame with variable x (for location) and two or more
#'          measurements
#' @return value between 0 and 1, ratio of length of overlap compared to
#'           smaller length of the signature
#' @importFrom assertthat assert_that
#' @export
extract_feature_overlap <- function(aligned) {
  assert_that(dim(aligned)[2] > 2, msg = "aligned must have at least 3 columns")
  for (i in 2:dim(aligned)[2]) {
    assert_that(
      is.numeric(aligned[, i]),
      msg = sprintf("Column %d (%s) is not numeric", i, names(aligned)[i]))
  }

  # compute non-missing overlap of the first two signatures
  sum(!is.na(aligned[, 2]) & !is.na(aligned[, 3])) / extract_feature_length(aligned)
}

#' Extract features from aligned signatures
#'
#' @param aligned aligned signatures, result from `sig_cms_max`
#' @param striae data frame with evaluated matching striae
#' @param ... passed on to extractor functions
#' XXX this needs some fixing
#' @importFrom utils apropos getFromNamespace
#' @importFrom tidyr spread
#' @importFrom assertthat assert_that
#' @export
extract_features_all <- function(aligned, striae, ...) {
  # figure out all the different functions, then figure out the format
  # What happens when ... contains arguments which are not needed for w/e fcn?
  feature <- value <- NULL
  assert_that(!is.null(aligned), !is.null(striae),
              msg = "aligned and striae must not be NULL")

  features <- apropos("extract_feature_")
  values <- features %>% purrr::map_dbl(.f = function(f) {
    fun <- getFromNamespace(f, asNamespace("bulletxtrctr"))
    if ("aligned" %in% names(formals(fun))) {
      res <- fun(aligned$bullets, ...)
    }
    if ("striae" %in% names(formals(fun))) {
      res <- fun(striae$lines, ...)
    }
    res
  })

  data.frame(
    feature = gsub("extract_feature_", "", features),
    value = values
  ) %>% spread(feature, value)
}

#' Extract features from aligned signatures (legacy)
#'
#' @param res list consisting of data frames of lines and aligned signatures,
#'          result from `sig_cms_max`
#' XXX this needs some fixing
#' @return data frame with variables ccf, rough_cor, D, sd_D, matches,
#'           mismatches, cms, non_cms, and sum_peaks
#' @export
extract_features_all_legacy <- function(res) {
  # browser()
  avgl30 <- bullet <- l30 <- sig1 <- sig2 <- smoothavgl30 <- type <- x <- NULL

  lofX <- res$bullets
  #    lofX$l30 <- lofX$sig
  #    b12 <- unique(lofX$bullet)
  b12 <- c("sig1", "sig2")
  subLOFx1 <- lofX[, c("x", "sig1")]
  names(subLOFx1) <- c("y", "val")
  subLOFx2 <- lofX[, c("x", "sig2")]
  names(subLOFx2) <- c("y", "val")
  #    browser()

  #   subLOFx1 <- subset(lofX, bullet==b12[1])
  #  subLOFx2 <- subset(lofX, bullet==b12[2])

  ys <- dplyr::intersect(
    round(subLOFx1$y, digits = 3),
    round(subLOFx2$y, digits = 3)
  )

  idx1 <- which(round(subLOFx1$y, digits = 3) %in% ys)
  idx2 <- which(round(subLOFx2$y, digits = 3) %in% ys)

  g1_inc_x <- 1.5625

  sq <- function(x) x^2

  distr.dist <- ((subLOFx1$val[idx1] - subLOFx2$val[idx2]) * g1_inc_x / 1000) %>%
    sq() %>%
    mean(na.rm = TRUE) %>%
    sqrt()
  distr.sd <- sd(subLOFx1$val * g1_inc_x / 1000, na.rm = TRUE) +
    sd(subLOFx2$val * g1_inc_x / 1000, na.rm = TRUE)

  km <- which(res$lines$match)
  knm <- which(!res$lines$match)
  if (length(km) == 0) km <- c(length(knm) + 1, 0)
  if (length(knm) == 0) knm <- c(length(km) + 1, 0)

  signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))

  doublesmoothed <- lofX %>%
    tidyr::gather(bullet, l30, sig1:sig2) %>%
    #   group_by(y) %>%
    mutate(avgl30 = mean(l30, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      smoothavgl30 = smoothloess(y = avgl30, span = 0.3),
      l50 = l30 - smoothavgl30
    )

  final_doublesmoothed <- doublesmoothed %>%
    filter(round(x, digits = 3) %in% ys)

  rough_cor <- cor(
    final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[1]],
    final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[2]],
    use = "pairwise.complete.obs"
  )

  data.frame(
    ccf = res$ccf,
    rough_cor = rough_cor,
    lag = res$lag / 1000,
    D = distr.dist,
    sd_D = distr.sd,
    #     b1=b12[1], b2=b12[2],
    signature_length = signature.length * g1_inc_x / 1000,
    overlap = length(ys) / signature.length,
    matches = sum(res$lines$match) * (1000 / g1_inc_x) / length(ys),
    mismatches = sum(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    cms = res$maxCMS * (1000 / g1_inc_x) / length(ys),
    cms2 = get_longest_run(
      subset(res$lines, type == 1 | is.na(type))$match
    ) * (1000 / g1_inc_x) / length(ys),
    non_cms = get_longest_run(!res$lines$match) * 1000 /
      abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    left_cms = max(knm[1] - km[1], 0) * (1000 / g1_inc_x) / length(ys),
    right_cms = max(km[length(km)] - knm[length(knm)], 0) *
      (1000 / g1_inc_x) / length(ys),
    left_noncms = max(km[1] - knm[1], 0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    right_noncms = max(knm[length(knm)] - km[length(km)], 0) *
      1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    sum_peaks = sum(abs(res$lines$heights[res$lines$match]), na.rm = TRUE) *
      (1000 / g1_inc_x) / length(ys)
  )
}
