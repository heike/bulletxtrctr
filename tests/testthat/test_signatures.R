context("signatures")


library(x3ptools)
library(bulletxtrctr)
library(tidyverse)
# b1 <- read_bullet("../../data/Bullet1", "x3p") %>%
# # turn the scans such that (0,0) is bottom left
#   mutate(
#     x3p = x3p %>% purrr::map(.f = function(x) x %>%
#                                rotate_x3p(angle=-90) %>%
#                                y_flip_x3p())
#   ) %>% mutate(
#     x3p = x3p %>% purrr::map(.f = function(x) {
#       # make sure all measurements are in microns
#       x$surface.matrix <- x$surface.matrix*10^6
#       x$header.info$incrementY <- x$header.info$incrementY*10^6
#       x$header.info$incrementX <- x$header.info$incrementX*10^6
#       x
#     })
#   ) %>%
#   filter(row_number() == 1) %>%
#   mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
#   mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
#   mutate(grooves = purrr::map(ccdata, cc_locate_grooves, method = "middle")) %>%
#   mutate(
#     sigs = purrr::map2(
#       .x = ccdata, .y = grooves,
#       .f = function(x, y) {
#         cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)})
#   )
#
# save(b1, file = "../../tests/testdata/correct_data_test_signatures.Rdata")

load("../testdata/correct_data_test_signatures.Rdata")

b2 <- read_bullet("../../data/Bullet1", "x3p") %>%
  # turn the scans such that (0,0) is bottom left
  mutate(
    x3p = x3p %>% purrr::map(.f = function(x) x %>%
                               rotate_x3p(angle=-90) %>%
                               y_flip_x3p())
  ) %>% mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
      # make sure all measurements are in microns
      x$surface.matrix <- x$surface.matrix*10^6
      x$header.info$incrementY <- x$header.info$incrementY*10^6
      x$header.info$incrementX <- x$header.info$incrementX*10^6
      x
    })
  ) %>%
  filter(row_number() == 1) %>%
  mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
  mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
  mutate(grooves = purrr::map(ccdata, cc_locate_grooves, method = "middle")) %>%
  mutate(
    sigs = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)}),
    sigs1 = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.01)}),
    sigs2 = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.25, span2=0.03)}),
    sigs3 = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.25, span2=0.01)})
  )

test_that("signatures works as expected", {
  expect_s3_class(b2$sigs[[1]], "data.frame")
  expect_equal(names(b2$sigs[[1]]), c("x", "y", "value", "raw_sig", "se", "sig"))
  expect_type(b2$sigs[[1]]$x, "double")
  expect_type(b2$sigs[[1]]$y, "double")
  expect_type(b2$sigs[[1]]$value, "double")
  expect_type(b2$sigs[[1]]$raw_sig, "double")
  expect_type(b2$sigs[[1]]$se, "double")
  expect_type(b2$sigs[[1]]$sig, "double")
  expect_length(unique(b2$sigs[[1]]$y), 1)
})

test_that("signatures is numerically correct", {
  expect_identical(b1$sigs, b2$sigs)
  expect_error(expect_identical(b2$sigs[[1]]$sig, b2$sigs1[[1]]$sig))
  expect_error(expect_identical(b2$sigs[[1]]$sig, b2$sigs2[[1]]$sig))
  expect_error(expect_identical(b2$sigs[[1]]$sig, b2$sigs3[[1]]$sig))
})

