context("read")

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
# save(b1, file = here::here("tests/testdata/correct_data_test_read.Rdata"))

load(here::here("tests/testdata/correct_data_test_read.Rdata"))
o1 <- capture.output(b2 <- read_bullet(here::here("data/Bullet1"), "x3p"), split = T)
b3 <- b2 %>%
  mutate(x3p = x3p %>% purrr::map(.f = function(x) {
    x %>%
      rotate_x3p(angle = -90) %>%
      y_flip_x3p()
  })) %>%
  mutate(x3p = x3p %>% purrr::map(.f = function(x) {
    # make sure all measurements are in microns
    x$surface.matrix <- x$surface.matrix*10^6
    x$header.info$incrementY <- x$header.info$incrementY*10^6
    x$header.info$incrementX <- x$header.info$incrementX*10^6
    x
  })) %>%
  filter(row_number() == 1)

test_that("read works as expected", {
  expect_length(o1, 0)
  expect_equivalent(nrow(b2), 6)
  expect_s3_class(b2, "tbl_df")
  expect_s3_class(b2, "tbl")
  expect_s3_class(b2, "data.frame")
  expect_identical(b1, b3)
})

