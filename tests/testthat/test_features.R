context("features")
skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/bullets_match.Rdata"))
  load(here::here("tests/single_features.Rdata"))

  featurestest_legacy <- extract_features_all_legacy(match$maxcms)
  featurestest_single <-   features_single <- data.frame(
    rightcms = extract_feature_right_cms(striae = match$maxcms$lines),
    leftcms = extract_feature_left_cms(striae = match$maxcms$lines),
    cms2 = extract_feature_cms2(striae = match$maxcms$lines),
    cms = extract_feature_cms(striae = match$maxcms$lines),
    noncms = extract_feature_non_cms(striae = match$maxcms$lines),
    matches = extract_feature_matches(striae = match$maxcms$lines),
    mismatches = extract_feature_mismatches(striae = match$maxcms$lines)
  )
  classes <- lapply(featurestest_legacy, mode) %>% unlist %>% unique
}

test_that("features works as expected", {
  skip_if(skipall)
  expect_s3_class(featurestest_legacy, "data.frame")
  expect_equal(classes, "numeric")
  expect_equal(featurestest_legacy, match$features_legacy)
})

test_that("extract_feature_right_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) > 11) %>%
      extract_feature_right_cms(),
    0)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) > 4) %>%
      extract_feature_right_cms(),
    6)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) > 0) %>%
      extract_feature_right_cms(),
    10)
  expect_equal(featurestest_single$rightcms, features_single$rightcms)
})

test_that("extract_feature_left_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 11) %>%
      extract_feature_left_cms(),
    10)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 5) %>%
      extract_feature_left_cms(),
    4)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0) %>%
      extract_feature_left_cms(),
    0)
  expect_equal(featurestest_single$leftcms, features_single$leftcms)
})


test_that("extract_feature_cms2 works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms2(),
    5)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 5,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms2(),
    2)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms2(),
    0)
  expect_equal(featurestest_single$cms2, features_single$cms2)
})

test_that("extract_feature_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms(),
    10)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 5,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms(),
    4)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms(),
    0)
  expect_equal(featurestest_single$cms, features_single$cms)
})

test_that("extract_feature_non_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_non_cms(),
    0)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 5,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_non_cms(),
    6)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_non_cms(),
    10)
  expect_equal(featurestest_single$noncms, features_single$noncms)
})

test_that("extract_feature_n_striae works as expected", {
  expect_equal(
    data.frame(match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_n_striae(type = "peak", match = T),
    5)
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_n_striae(type = "peak", match = T),
    3)
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_n_striae(type = "valley", match = T),
    4)
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_n_striae(type = "peak", match = F),
    2)
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_n_striae(type = "valley", match = F),
    1)
})

test_that("extract_feature_matches works as expected", {
  expect_equal(
    data.frame(match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_matches(),
    10)
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_matches(),
    7)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_matches(),
    0)
  expect_equal(featurestest_single$matches, features_single$matches)
})

test_that("extract_feature_mismatches works as expected", {
  expect_equal(
    data.frame(match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_mismatches(),
    0)
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_mismatches(),
    3)
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0,
               type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_mismatches(),
    10)
  expect_equal(featurestest_single$mismatches, features_single$mismatches)
})

test_that("extract_feature_ccf works as expected", {

})
