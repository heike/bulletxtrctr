context("features")

library(x3ptools)
library(tidyverse)

load(here::here("tests/testdata/correct_data_test_cms.Rdata"))

# featuresorig <- extract_features_all(maxcms)
# save(featuresorig, file = here::here("tests/testdata/correct_data_extract_features.Rdata"))

featurestest <- extract_features_all(maxcms)
load(here::here("tests/testdata/correct_data_extract_features.Rdata"))

test_that("features works as expected", {
  expect_s3_class(featuresorig, "data.frame")

  classes <- lapply(featuresorig, mode) %>% unlist %>% unique
  expect_equal(classes, "numeric")

  expect_equal(featurestest, featuresorig)
})
