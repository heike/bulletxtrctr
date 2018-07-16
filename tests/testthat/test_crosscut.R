context("crosscut")

library(x3ptools)
library(bulletxtrctr)
library(tidyverse)

# b1 <- read_bullet("data/Bullet1", "x3p")
# # turn the scans such that (0,0) is bottom left
# b1 <- b1 %>% mutate(
#   x3p = x3p %>% purrr::map(.f = function(x) x %>%
#                              rotate_x3p(angle=-90) %>%
#                              y_flip_x3p())
# ) %>% mutate(
#   x3p = x3p %>% purrr::map(.f = function(x) {
#     # make sure all measurements are in microns
#     x$surface.matrix <- x$surface.matrix*10^6
#     x$header.info$incrementY <- x$header.info$incrementY*10^6
#     x$header.info$incrementX <- x$header.info$incrementX*10^6
#     x
#   })
# ) %>%
#   filter(row_number() == 1)
#
#
# b1_l1 <- x3ptools::x3p_to_df(b1$x3p[[1]])
#
# save(b1, b1_l1, file = "tests/testdata/crosscut_test_data.Rdata")

load("../testdata/crosscut_test_data.Rdata")
cc <- bulletxtrctr:::land_cc(50, b1_l1)

test_that("land_cc works as expected", {
  expect_s3_class(cc, "data.frame")
  expect_equal(names(cc), c("x", "y", "value", "fitted", "resid", "se", "abs_resid", "chop"))
})

test_that("x3p_crosscut_optimize works as expected", {
  expect_silent(cco <- x3p_crosscut_optimize(b1$x3p[[1]]))
  expect_gte(cco, 50)
  expect(is.numeric(cco))
})

test_that("x3p_crosscut works as expected", {
  expect_silent(cc <- x3p_crosscut(b1$x3p[[1]], x3p_crosscut_optimize(b1$x3p[[1]])))
  expect_s3_class(cc, "data.frame")
  expect_equal(names(cc), c("x", "y", "value"))
  expect_length(attr(cc, "header.info"), 4)
  expect_equal(sort(names(attr(cc, "header.info"))), sort(c("sizeY", "sizeX", "incrementY", "incrementX")))
})

