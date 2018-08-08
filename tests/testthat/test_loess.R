context("loess")

library(x3ptools)
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
#   filter(row_number() == 1) %>%
#   mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
#   mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
#   mutate(loess = purrr::map(ccdata, cc_fit_loess, span = .75),
#          loess2 = purrr::map(ccdata, cc_fit_loess, span = .25),
#          gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600),
#          gauss2 = purrr::map(ccdata, cc_fit_gaussian, span = 300))
#
#
# save(b1, file = here::here("tests/testdata/correct_data_test_loess.Rdata"))

load(here::here("tests/testdata/correct_data_test_loess.Rdata"))
b2 <- read_bullet(here::here("data/Bullet1"), "x3p") %>%
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
  mutate(loess = purrr::map(ccdata, cc_fit_loess, span = .75),
         loess2 = purrr::map(ccdata, cc_fit_loess, span = .25),
         gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600),
         gauss2 = purrr::map(ccdata, cc_fit_gaussian, span = 300))

ex_names <- c("x", "y", "value", "fitted", "raw_sig", "se", "abs_resid", "chop")

test_that("loess works as expected", {
  expect_s3_class(b2$loess[[1]], "data.frame")
  expect_equivalent(names(b2$loess[[1]]), ex_names)
  expect_equivalent(summarize_all(b2$loess[[1]], mode) %>% as.character(),
                   c(rep("numeric", 7), "logical"))
  expect_equivalent(b1$loess, b2$loess)
  expect_equivalent(b1$loess2, b2$loess2)
  expect_equivalent(b1$gauss, b2$gauss)
  expect_equivalent(b1$gauss2, b2$gauss2)
})
