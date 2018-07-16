context("read")

library(bulletxtrctr)
library(tidyverse)

test_that("read works as expected", {
  o1 <- capture.output(b1 <- read_bullet("../../data/Bullet1", "x3p"), split = T)
  expect_equivalent(nrow(b1), 6)
  expect_s3_class(b1, "tbl_df")
  expect_s3_class(b1, "tbl")
  expect_s3_class(b1, "data.frame")
  expect_length(o1, 0)
})
