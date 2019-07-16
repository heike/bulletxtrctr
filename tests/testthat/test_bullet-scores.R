context("bullet-scores")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr") &
    requireNamespace("randomForest")) {
  skipall <- F
  load(here::here("tests/rf_features.Rdata"))
  test_rf_features <- predict(bulletxtrctr::rtrees, newdata = rf_features,
                              type = "prob")[, 2]
}

test_that("bullet-scores works as expected", {
  expect_equal(
    compute_average_scores(
      rep(1:2, times = 2), rep(1:2, each = 2),
      rep(1:2, times = 2) * rep(1:2, each = 2)
    ),
    c(2.5, 2.0)
  )
  skip_if(skipall)
  expect_equal(
    compute_average_scores(2, 4, test_rf_features),
    compute_average_scores(2, 4, rf_features$rfscore)
  )
})

test_that("bullet_to_land_predict works as expected", {
  df <- data.frame(x = rep(1:6, each = 6), y = rep(1:6, times = 6))
  res <- bullet_to_land_predict(df$x, df$y, rnorm(df$x, as.numeric(df$x == df$y), sd = .2), difference = .1)
  expect_equal(matrix(res, nrow = 6, ncol = 6) %>% diag(), rep(TRUE, 6))
})
