#' Extract ccf from two (or more) aligned signatures
#'
#' @param aligned data frame with variable x (for location) and two or more measurements
#' @return (matrix) of correlations
#' @export
extract_feature_ccf <- function(aligned) {
  if (dim(aligned)[2] == 3) # only two signatures to compare
    return(cor(aligned[,2], aligned[,3], use="pairwise"))
  cor(aligned[,-1])
}

#' Extract lag from two (or more) aligned signatures
#'
#' In the case of two signatures the result is an integer definining the number of indices by which the second signature is shifted compared to the first signature in the alignment. Note that this lag can be negative.
#' In the case of multiple signatures the result is a vector of non-negative integers of the same length as signatures. Each element gives the number of indices by which the corresponding signature is shifted compared to the *first* signature. By definition, one of the numbers has to be 0 indicating the *first* signature.
#' @param aligned data frame with variable x (for location) and two or more measurements
#' @return (vector) of lags
#' @export
extract_feature_lag <- function(aligned) {
  lags <- sapply(aligned[,-1], function(x) {
    if (!is.na(x[1])) return(0)
    diffs <- diff(is.na(x))
    which(diffs==-1)
  })

  if (length(lags) == 2) return(diff(lags))
  lags
}

#' Extract features from aligned signatures
#'
#' @param res list consisting of data frames of lines and aligned signatures, result from `sig_cms_max`
#' XXX this needs some fixing
#' @export
extract_features_all <- function(res) {
  #browser()
  avgl30 <- bullet <- l30 <- sig1 <- sig2 <- smoothavgl30 <- type <- x <- NULL

  lofX <- res$bullets
  #    lofX$l30 <- lofX$sig
  #    b12 <- unique(lofX$bullet)
  b12 <- c("sig1", "sig2")
  subLOFx1 <- lofX[,c("x", "sig1")]
  names(subLOFx1) <- c("y", "val")
  subLOFx2 <- lofX[,c("x", "sig2")]
  names(subLOFx2) <- c("y", "val")
  #    browser()

  #   subLOFx1 <- subset(lofX, bullet==b12[1])
  #  subLOFx2 <- subset(lofX, bullet==b12[2])

  ys <- dplyr::intersect(round(subLOFx1$y, digits = 3), round(subLOFx2$y, digits = 3))

  idx1 <- which(round(subLOFx1$y, digits = 3) %in% ys)
  idx2 <- which(round(subLOFx2$y, digits = 3) %in% ys)

  g1_inc_x <- 1.5625

  distr.dist <- sqrt(mean(((subLOFx1$val[idx1] - subLOFx2$val[idx2]) * g1_inc_x / 1000)^2, na.rm=TRUE))
  distr.sd <- sd(subLOFx1$val * g1_inc_x / 1000, na.rm=TRUE) + sd(subLOFx2$val * g1_inc_x / 1000, na.rm=TRUE)

  km <- which(res$lines$match)
  knm <- which(!res$lines$match)
  if (length(km) == 0) km <- c(length(knm)+1,0)
  if (length(knm) == 0) knm <- c(length(km)+1,0)

  signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))

  doublesmoothed <- lofX %>%
    tidyr::gather(bullet, l30, sig1:sig2) %>%
    #   group_by(y) %>%
    mutate(avgl30 = mean(l30, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(smoothavgl30 = smoothloess(y = avgl30, span = 0.3),
           l50 = l30 - smoothavgl30)

  final_doublesmoothed <- doublesmoothed %>%
    filter(round(x, digits = 3) %in% ys)

  rough_cor <- cor(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[1]],
                   final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[2]],
                   use = "pairwise.complete.obs")

  data.frame(
    ccf=res$ccf,
    rough_cor = rough_cor,
    lag=res$lag / 1000,
    D=distr.dist,
    sd_D = distr.sd,
    #     b1=b12[1], b2=b12[2],
    signature_length = signature.length * g1_inc_x / 1000,
    overlap = length(ys) / signature.length,
    matches = sum(res$lines$match) * (1000 / g1_inc_x) / length(ys),
    mismatches = sum(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    cms = res$maxCMS * (1000 / g1_inc_x) / length(ys),
    cms2 = get_longest_run(subset(res$lines, type==1 | is.na(type))$match) * (1000 / g1_inc_x) / length(ys),
    non_cms = get_longest_run(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    left_cms = max(knm[1] - km[1], 0) * (1000 / g1_inc_x) / length(ys),
    right_cms = max(km[length(km)] - knm[length(knm)],0) * (1000 / g1_inc_x) / length(ys),
    left_noncms = max(km[1] - knm[1], 0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    right_noncms = max(knm[length(knm)]-km[length(km)],0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
    sum_peaks = sum(abs(res$lines$heights[res$lines$match]), na.rm=TRUE) * (1000 / g1_inc_x) / length(ys)
  )
}
