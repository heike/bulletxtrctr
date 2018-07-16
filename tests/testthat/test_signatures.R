context("signatures")


library(x3ptools)
library(bulletxtrctr)
library(tidyverse)

load("../testdata/crosscut_test_data.Rdata")
b1 <- b1 %>% mutate(
  crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
)
# now extract the crosscuts
b1 <- b1 %>%
  mutate(
    ccdata = purrr::map2(.x = x3p, .y = crosscut,
                         .f = x3p_crosscut)
  ) %>%
  # Get grooves
  mutate(
    grooves = ccdata %>% purrr::map(.f = cc_locate_grooves, method = "middle")
  )

test_that("signatures works as expected", {
  b1 <- b1 %>% mutate(
    sigs = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)})
  )

  expect_s3_class(b1$sigs[[1]], "data.frame")
  expect_equal(names(b1$sigs[[1]]), c("x", "y", "value", "raw_sig", "se", "sig"))
  expect_type(b1$sigs[[1]]$x, "double")
  expect_type(b1$sigs[[1]]$y, "double")
  expect_type(b1$sigs[[1]]$value, "double")
  expect_type(b1$sigs[[1]]$raw_sig, "double")
  expect_type(b1$sigs[[1]]$se, "double")
  expect_type(b1$sigs[[1]]$sig, "double")
  expect_length(unique(b1$sigs[[1]]$y), 1)
})
