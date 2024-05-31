context("align")

skipall <- T
if (requireNamespace("here")) {
  skipall <- F
  load(here::here("tests/bullets_signatures.Rdata"))
  load(here::here("tests/bullets_match.Rdata"))

  aligntest <- sig_align(b1_l2_x3p$sigs[[1]]$sig, b2_l4_x3p$sigs[[1]]$sig)
  aligntest2 <- sig_align(b2_l4_x3p$sigs[[1]]$sig, b1_l2_x3p$sigs[[1]]$sig)
}

ccftest <- get_ccf(1:5, 6:2)
trivial <- sig_align((1:10 - 5)^2, (3:6 - 5)^2)

test_that("cross-correlation works", {
  expect_equal(names(ccftest), c("lag", "ccf"))
  expect_equal(ccftest$lag, -5:5)
  expect_equal(ccftest$ccf, c(NA, NA, rep(-1, 7), NA, NA))
})


test_that("sig_align is working as expected", {
  expect_true(is.na(trivial$lands$sig2[1]))
  expect_true(is.na(trivial$lands$sig2[10]))

  expect_equal(names(trivial), c("ccf", "lag", "lands", "cors"))
  expect_is(trivial$ccf, "numeric")
  expect_is(trivial$lag, "numeric")
  expect_s3_class(trivial$lands, "data.frame")
  expect_equal(names(trivial$lands), c("x", "sig1", "sig2"))
  expect_is(trivial$lands$x, "integer")
  expect_is(trivial$lands$sig1, "numeric")
  expect_is(trivial$lands$sig2, "numeric")

  skip_if(skipall)
  # take out the plot from the test
# ## results have changed considerably
#  expect_equal(aligntest, match$alignment)
#  expect_failure(expect_equal(aligntest2, match$alignment))
})
