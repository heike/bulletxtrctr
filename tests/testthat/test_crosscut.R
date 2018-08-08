context("crosscut")

library(x3ptools)
library(bulletxtrctr)
library(tidyverse)

# b1 <- read_bullet(here::here("data/Bullet1"), "x3p") %>%
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
#   filter(row_number() == 1)
#
# b1_l1 <- x3ptools::x3p_to_df(b1$x3p[[1]])
# cc1 <- bulletxtrctr:::land_cc(50, b1_l1)
# fullcc1 <- x3p_crosscut(b1$x3p[[1]], x3p_crosscut_optimize(b1$x3p[[1]]))
# cco1 <- x3p_crosscut_optimize(b1$x3p[[1]])
#
# save(b1_l1, cc1, cco1, fullcc1, file = here::here("tests/testdata/correct_data_test_crosscut.Rdata"))

load(here::here("tests/testdata/correct_data_test_crosscut.Rdata"))


b2 <- read_bullet(here::here("data/Bullet1"),  "x3p") %>%
  mutate(x3p = x3p %>% purrr::map(.f = function(x) x %>% rotate_x3p(angle = -90) %>% y_flip_x3p)) %>%
  mutate(x3p = x3p %>% purrr::map(.f = function(x) {
    # make sure all measurements are in microns
    x$surface.matrix <- x$surface.matrix*10^6
    x$header.info$incrementY <- x$header.info$incrementY*10^6
    x$header.info$incrementX <- x$header.info$incrementX*10^6
    x
  })) %>%
  filter(row_number() == 1)
b2_l1 <- x3ptools::x3p_to_df(b2$x3p[[1]])
cc2 <- bulletxtrctr:::land_cc(50, b2_l1)


test_that("land_cc works as expected", {
  expect_s3_class(cc2, "data.frame")
  expect_equal(names(cc2), c("x", "y", "value", "fitted", "raw_sig", "se", "abs_resid", "chop",  "resid"))
  expect_equivalent(cc1, cc2)
})

cco2 <- x3p_crosscut_optimize(b2$x3p[[1]])

test_that("x3p_crosscut_optimize works as expected", {
  expect_silent(x3p_crosscut_optimize(b2$x3p[[1]]))
  expect_gte(cco2, 50)
  expect(is.numeric(cco2))
  expect_equivalent(cco1, cco2)
})

fullcc2 <- x3p_crosscut(b2$x3p[[1]], x3p_crosscut_optimize(b2$x3p[[1]]))
test_that("x3p_crosscut works as expected", {
  expect_silent(x3p_crosscut(b2$x3p[[1]], x3p_crosscut_optimize(b2$x3p[[1]])))
  expect_s3_class(fullcc2, "data.frame")
  expect_equal(names(fullcc2), c("x", "y", "value"))
  expect_length(attr(fullcc2, "header.info"), 4)
  expect_equal(sort(names(attr(fullcc2, "header.info"))), sort(c("sizeY", "sizeX", "incrementY", "incrementX")))
  expect_equivalent(fullcc2, fullcc1)
})
