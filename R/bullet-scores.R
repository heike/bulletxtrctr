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
