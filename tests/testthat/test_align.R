context("align")

load(here::here("tests/bullets_signatures.Rdata"))
load(here::here("tests/bullets_match.Rdata"))

ccftest <- get_ccf(1:5, 6:2)
aligntest <- sig_align(b1_l3_x3p$sigs[[1]]$sig, b2_l5_x3p$sigs[[1]]$sig)

test_that("cross-correlation works", {
  expect_equal(names(ccftest), c("lag", "ccf"))
  expect_equal(ccftest$lag, -5:5)
  expect_equal(ccftest$ccf, c(NA, NA, rep(-1, 7), NA, NA))
})


test_that("sig_align is working as expected", {
  expect_equal(names(aligntest), c("ccf", "lag", "bullets"))
  expect_is(aligntest$ccf, "numeric")
  expect_is(aligntest$lag, "numeric")
  expect_s3_class(aligntest$bullets, "data.frame")
  expect_equal(names(aligntest$bullets), c("x", "sig1", "sig2"))
  expect_is(aligntest$bullets$x, "integer")
  expect_is(aligntest$bullets$sig1, "numeric")
  expect_is(aligntest$bullets$sig2, "numeric")
  expect_equal(aligntest, match$alignment)
})
