context("grooves")

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
#   filter(row_number() == 1) %>%
#   mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
#   mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
#   mutate(grooves = purrr::map(ccdata, cc_locate_grooves)) %>%
#   mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves, method = "middle"))
#
# save(b1, file = here::here("tests/testdata/correct_data_test_grooves.Rdata"))

load("../testdata/correct_data_test_grooves.Rdata")

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
  mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
  mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves, method = "middle", return_plot = T))

test_that("grooves works as expected", {
  expect_silent(cc_locate_grooves(b2$ccdata[[1]]))
  expect_length(b2$grooves[[1]], 2)
  expect_equal(names(b2$grooves[[1]]), c("groove", "plot"))
  expect_s3_class(b2$grooves[[1]]$plot, "ggplot")

  tmp <- cc_locate_grooves(b2$ccdata[[1]])
  expect_length(tmp, 1)
  expect_length(tmp$groove, 2)
  expect_is(tmp$groove, "numeric")

  expect_silent(cc_locate_grooves(b2$ccdata[[1]], "middle"))
  expect_length(b2$grooves_mid[[1]], 2)
  expect_equal(names(b2$grooves_mid[[1]]), c("groove", "plot"))
  expect_s3_class(b2$grooves_mid[[1]]$plot, "ggplot")

  ## What other groove methods should be tested?
})

test_that("grooves is numerically correct", {
  # Not identical because of plots
  # expect_equal(b1$grooves, b2$grooves)
  # expect_equal(b1$grooves_mid, b2$grooves_mid)

  # Check numerically identical for groove locations, at least...
  expect_identical(b1$grooves[[1]]$groove, b2$grooves[[1]]$groove)
  expect_identical(b1$grooves_mid[[1]]$groove, b2$grooves_mid[[1]]$groove)
})
