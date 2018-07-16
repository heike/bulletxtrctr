context("grooves")

library(x3ptools)
library(bulletxtrctr)
library(tidyverse)
#
# b1 <- read_bullet("../testdata/Bullet1", "x3p")
# b1_l1 <- x3ptools::x3p_to_df(b1$x3p[[1]])

load("../testdata/crosscut_test_data.Rdata")

test_that("grooves works as expected", {
  b1 <- b1 %>% mutate(
    crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
  )
  # now extract the crosscuts
  b1 <- b1 %>% mutate(
    ccdata = purrr::map2(.x = x3p, .y = crosscut,
                         .f = x3p_crosscut)
  )
  expect_silent(res <- cc_locate_grooves(b1$ccdata[[1]]))
  expect_length(res$groove, 2)
  expect_equal(names(res), c("groove", "plot"))
  expect_s3_class(res$plot, "ggplot")

  expect_silent(res2 <- cc_locate_grooves(b1$ccdata[[1]], "middle"))
  expect_length(res$groove, 2)
  expect_equal(names(res2), c("groove", "plot"))
  expect_s3_class(res2$plot, "ggplot")

  ## What other groove methods should be tested?
})
