context("cms")


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
# maxcms <- sig_cms_max(b1$sigs[[3]]$sig, b2$sigs[[5]]$sig)
#
# save(maxcms, file = "../../tests/testdata/correct_data_test_cms.Rdata")

load("../testdata/correct_data_test_cms.Rdata")
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

maxcmstest <- sig_cms_max(b1$sigs[[3]]$sig, b2$sigs[[5]]$sig)


test_that("sig_cms_max returns correctly structured data", {
  expect_equal(names(maxcmstest), c("maxCMS", "ccf", "lag", "lines", "bullets"))
  expect_equal(names(maxcmstest$lines), c("xmin", "xmax", "match", "type", "meany", "heights", "sdheights"))
  expect_equal(names(maxcmstest$bullets), c("x", "sig1", "sig2"))
  expect_is(maxcmstest$maxCMS, "numeric")
  expect_is(maxcmstest$ccf, "numeric")
  expect_is(maxcmstest$lag, "numeric")
  expect_s3_class(maxcmstest$lines, "data.frame")
  expect_s3_class(maxcmstest$bullets, "data.frame")
  expect_is(maxcmstest$lines$xmin, "numeric")
  expect_is(maxcmstest$lines$xmax, "numeric")
  expect_is(maxcmstest$lines$match, "logical")
  expect_is(maxcmstest$lines$type, "numeric")
  expect_is(maxcmstest$lines$meany, "numeric")
  expect_is(maxcmstest$lines$heights, "numeric")
  expect_is(maxcmstest$lines$sdheights, "numeric")
  expect_is(maxcmstest$bullets$x, "integer")
  expect_is(maxcmstest$bullets$sig1, "numeric")
  expect_is(maxcmstest$bullets$sig2, "numeric")

})


test_that("sig_cms_max returns numerically correct results", {
  expect_equal(maxcmstest, maxcms)
  })

test_that(
  "get_runs works as expected",
  {
    x <- c(rep(0, 5), rep(1, 3), 0, rep(1, 6))
    y <- as.table(c("3" = 1, "6" = 1))
    dimnames(y) <- list(c("3", "6"))
    expect_warning(get_runs(x), "Converting .* to a logical vector")
    expect_equivalent(get_runs(x == 1), y)

    expect_equal(get_longest_run(x == 1), 6)
  })

