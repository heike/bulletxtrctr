#' Get average scores for bullet to bullet comparisons
#'
#' Note that the combination of `land1` and `land2` are a key to the scores,
#' i.e. if a bullet has six lands, each of the input vectors should have
#' length 36.
#' @param land1 (numeric) vector with land ids of bullet 1
#' @param land2 (numeric) vector with land ids of bullet 2
#' @param score numeric vector of scores to be summarized into a single number
#' @export
#' @importFrom readr parse_number
#' @importFrom stats xtabs
#' @import assertthat
#' @return numeric vector of average scores. Length is the same as the number of
#'           land engraved areas on the bullets.
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' bullets <- bullet_pipeline(
#'   location = list(
#'     Bullet1 = c(hamby252demo$bullet1),
#'     Bullet2 = c(hamby252demo$bullet2)
#'   ),
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' ) %>%
#' mutate(land = paste0(rep(1:2, each = 6), "-", rep(1:6, times = 2)))
#'
#' comparisons <- data.frame(
#'   expand.grid(land1 = bullets$land, land2 = bullets$land),
#'   stringsAsFactors = FALSE)
#' comparisons <- comparisons %>%
#'   mutate(
#'     aligned = purrr::map2(.x = land1, .y = land2, .f = function(xx, yy) {
#'       land1 <- bullets$sigs[bullets$land == xx][[1]]
#'       land2 <- bullets$sigs[bullets$land == yy][[1]]
#'       land1$bullet <- "first-land"
#'       land2$bullet <- "second-land"
#'
#'       sig_align(land1$sig, land2$sig)
#'     }),
#'     striae = purrr::map(aligned, sig_cms_max),
#'     features = purrr::map2(.x = aligned, .y = striae, extract_features_all),
#'     rfscore = purrr::map_dbl(features, rowMeans) # This is a hack until the new RF is fit...
#'   )
#'
#' # Clean up a bit
#' comparisons <- comparisons %>%
#'   mutate(
#'     bulletA = gsub("(\\d)-\\d", "\\1", land1),
#'     landA = gsub("\\d-(\\d)", "\\1", land1),
#'     bulletB = gsub("(\\d)-\\d", "\\1", land2),
#'     landB = gsub("\\d-(\\d)", "\\1", land2)
#'   ) %>%
#'   group_by(bulletA, bulletB) %>% tidyr::nest() %>%
#'   mutate(
#'     bullet_score = data %>% purrr::map_dbl(
#'       .f = function(d) max(compute_average_scores(land1 = d$landA,
#'                                                   land2 = d$landB,
#'                                                   d$rfscore)))
#'   )
#' }
compute_average_scores <- function(land1, land2, score) {
  land1 <- readr::parse_number(land1)
  land2 <- readr::parse_number(land2)
  assert_that(is.numeric(land1), is.numeric(land2), is.numeric(score))

  maxland <- max(land1, land2)
  fullframe <- data.frame(expand.grid(land1 = 1:maxland, land2 = 1:maxland))
  bcompare <- data.frame(land1, land2, score)

  fullframe <- fullframe %>% left_join(bcompare, by = c("land1", "land2"))

  fullframe <- fullframe %>% mutate(
    land1 = factor(land1, levels = 1:maxland),
    land2 = factor(land2, levels = 1:maxland)
  )
  # get averages, just in case
  matrix <- xtabs(score ~ land1 + land2,
    data = fullframe
  ) / xtabs(~land1 + land2, data = fullframe)

  matrix <- cbind(matrix, matrix)

  scores <- 1:maxland %>% sapply(FUN = function(i) {
    if (i == 1) {
      mean(diag(matrix), na.rm = TRUE)
    } else {
      i <- i - 1
      mean(diag(matrix[, -(1:i)]), na.rm = TRUE)
    }
  })
  scores
}


#' Helper function: bootstrap scores
#'
#' @param scores numeric values of scores
#' @param k number of lands to average across
#' @param K number of replicates
#' @param value observed value
bootstrap_k <- function(scores, k, K, value) {
  res <- replicate(K, {
    mean(sample(scores, size = k, replace = TRUE), na.rm = TRUE)
  })
  sum(res >= value)/K
}

#' Get land to land prediction based on bullet to bullet comparisons
#'
#' The combination of `land1` and `land2` are a key to the scores,
#' i.e. if a bullet has six lands, each of the input vectors should have
#' length 36.
#' @param land1 (numeric) vector with land ids of bullet 1
#' @param land2 (numeric) vector with land ids of bullet 2
#' @param scores numeric vector of scores to be summarized into a single number
#' @param difference numeric value describing the minimal difference between scores from same source versus different sources.
#' @export
#' @return numeric vector of binary prediction whether two lands are same-source. Vector has the same length as the input vectors.
bullet_to_land_predict <- function(land1, land2, scores, difference) {
    land1 <- as.numeric(land1)
    land2 <- as.numeric(land2)
    avgs <- compute_average_scores(land2, land1, scores)
    p <- max(c(land1, land2))
    if ( (diff(sort(-avgs))[1] > difference) & bootstrap_k(scores, p, 1000, max(avgs)) < 0.05) {
    # pick the maximum to determine the phase
    idx <- which.max(avgs)
    dd <- data.frame(
      land1 = parse_number(land1),
      land2 = parse_number(land2))
    dd$diff = (dd$land1 - dd$land2) %% p + 1


    return(dd$diff == idx)
    } else {

      return(rep(FALSE, length=length(land1)))
    }
  }
