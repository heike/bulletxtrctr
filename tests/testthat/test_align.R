context("align")


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
#   # filter(row_number() == 1) %>%
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
# b2 <- read_bullet("../../data/Bullet2", "x3p") %>%
#   # turn the scans such that (0,0) is bottom left
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
#   # filter(row_number() == 1) %>%
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
# alignment <- bulletxtrctr::sig_align(b1$sigs[[3]]$sig, b2$sigs[[5]]$sig)
#
# save(alignment, file = "../../tests/testdata/correct_data_test_align.Rdata")

test_that("cross-correlation works", {
  tmp <- get_ccf(1:5, 6:2)
  expect_equal(names(tmp), c("lag", "ccf"))
  expect_equal(tmp$lag, -5:5)
  expect_equal(tmp$ccf, c(NA, NA, rep(-1, 7), NA, NA))
})

load("../testdata/correct_data_test_align.Rdata")
b1test <- read_bullet("../../data/Bullet1", "x3p") %>%
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
  # filter(row_number() == 1) %>%
  mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
  mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
  mutate(grooves = purrr::map(ccdata, cc_locate_grooves, method = "middle")) %>%
  mutate(
    sigs = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)})
  )

b2test <- read_bullet("../../data/Bullet2", "x3p") %>%
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
  # filter(row_number() == 1) %>%
  mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
  mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
  mutate(grooves = purrr::map(ccdata, cc_locate_grooves, method = "middle")) %>%
  mutate(
    sigs = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)})
  )

aligntest <- bulletxtrctr::sig_align(b1test$sigs[[3]]$sig, b2test$sigs[[5]]$sig)

test_that("sig_align returns expected structure values", {
  expect_equal(names(aligntest), c("ccf", "lag", "bullets"))
  expect_is(aligntest$ccf, "numeric")
  expect_is(aligntest$lag, "numeric")
  expect_s3_class(aligntest$bullets, "data.frame")
  expect_equal(names(aligntest$bullets), c("x", "sig1", "sig2"))
  expect_is(aligntest$bullets$x, "integer")
  expect_is(aligntest$bullets$sig1, "numeric")
  expect_is(aligntest$bullets$sig2, "numeric")
})

test_that("sig_align is numerically correct",
          {
            expect_equal(aligntest, alignment)
          })
