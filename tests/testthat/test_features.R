context("features")
skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/bullets_match.Rdata"))

  featurestest <- extract_features_all(match$maxcms)
  classes <- lapply(featurestest, mode) %>% unlist %>% unique
}

test_that("features works as expected", {
  skip_if(skipall)
  expect_s3_class(featurestest, "data.frame")
  expect_equal(classes, "numeric")
  expect_equal(featurestest, match$features)
})

