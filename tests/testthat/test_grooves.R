context("grooves")

library(x3ptools)
library(bulletxtrctr)
library(tidyverse)
#
# b1 <- read_bullet("../testdata/Bullet1", "x3p")
# b1_l1 <- x3ptools::x3p_to_df(b1$x3p[[1]])

load("../testdata/crosscut_test_data.Rdata")

test_that("grooves works as expected", {
  expect_message(res <- cc_locate_grooves(b1_l1), "summarizing \\d{1,} profiles by averaging across values")
  expect_length(res$groove, 2)
  expect_equal(names(res), c("groove", "plot"))
})
