context("bullet-scores")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr") & requireNamespace("randomForest")) {
  skipall <- F
  load(here::here("tests/rf_features.Rdata"))
  test_rf_features <- predict(bulletr::rtrees, newdata = rf_features, type = "prob")[,2]
}

test_that("bullet-scores works as expected", {
  expect_equal(
    compute_average_scores(rep(1:2, times = 2), rep(1:2, each = 2),
                           rep(1:2, times = 2)*rep(1:2, each = 2)),
    c(2.5, 2.0))
  skip_if(skipall)
  expect_equal(
    compute_average_scores(2, 4, test_rf_features),
    compute_average_scores(2, 4, rf_features$rfscore)
  )
})
