context("peaks")


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
# b2 <- read_bullet(here::here("data/Bullet2"), "x3p") %>%
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
# bAlign <- sig_align(b1$sigs[[2]]$sig, b2$sigs[[4]]$sig)
#
# peaks <- sig_get_peaks(bAlign$bullet$sig1)
# peaks2 <- sig_get_peaks(bAlign$bullet$sig2)
# matches <- bulletxtrctr:::striation_identify_matches(peaks$lines, peaks2$lines)
#
# save(peaks, matches, file = here::here("tests/testdata/correct_data_test_peaks.Rdata"))

load(here::here("tests/testdata/correct_data_test_peaks.Rdata"))
b1test <- read_bullet(here::here("data/Bullet1"), "x3p") %>%
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

b2test <- read_bullet(here::here("data/Bullet2"), "x3p") %>%
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

bAligntest <- sig_align(b1test$sigs[[2]]$sig, b2test$sigs[[4]]$sig)

peakstest <- sig_get_peaks(bAligntest$bullet$sig1)
peakstest2 <- sig_get_peaks(bAligntest$bullet$sig2)

test_that("peaks works as expected", {
  expect_equal(names(peakstest), c("peaks", "valleys", "extrema", "peaks.heights",
                                   "valleys.heights", "lines", "plot", "dframe"))
  expect_is(peakstest$peaks, "integer")
  expect_is(peakstest$valleys, "integer")
  expect_is(peakstest$extrema, "integer")
  expect_is(peakstest$peaks.heights, "numeric")
  expect_equal(length(peakstest$peaks), length(peakstest$peaks.heights))
  expect_equal(length(peakstest$valleys), length(peakstest$valleys.heights))
  expect_equal(length(peakstest$peaks) + length(peakstest$valleys),
               length(peakstest$extrema))
  expect_equal(names(peakstest$lines), c("xmin", "xmax", "type", "extrema", "heights"))
  expect_equal(lapply(peakstest$lines, mode) %>% as.character(), rep("numeric", 5))
  expect_s3_class(peakstest$plot, "ggplot")
  expect_equal(names(peakstest$dframe), c("x", "smoothed"))
  expect_equal(lapply(peakstest$dframe, mode) %>% as.character(), c("numeric", "numeric"))
  expect_equal(peaks, peakstest)
})

matchestest <- striation_identify_matches(peakstest$lines, peakstest2$lines)

test_that("striation_identify_matches works as expected", {
  expect_equal(names(matchestest), c("xmin", "xmax", "match", "type",
                                     "meany", "heights", "sdheights"))
  expect_is(matchestest$xmin, "numeric")
  expect_is(matchestest$xmax, "numeric")
  expect_is(matchestest$match, "logical")
  expect_is(matchestest$meany, "numeric")
  expect_is(matchestest$heights, "numeric")
  expect_is(matchestest$sdheights, "numeric")
  expect_equal(matches, matchestest)
})
