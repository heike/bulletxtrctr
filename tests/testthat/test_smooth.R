context("smooth")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/smooth.Rdata"))
}

set.seed(3240583)
tmp <- dplyr::data_frame(
  x = seq(-sqrt(5), sqrt(5), .03) %>% jitter(),
  y = rnorm(length(x), x^2, .1)
)
smoothrestest <- smoothloess(tmp$y, .5)
sigsmoothrestest <- raw_sig_smooth(tmp$y, .5, c(-5, 5))

test_that("smooth functions works as expected", {
  expect_is(smoothrestest, "numeric")
  expect_is(sigsmoothrestest, "numeric")

  expect_equivalent(
    smoothrestest[smoothrestest < 5],
    sigsmoothrestest[smoothrestest < 5]
  )
  expect_equivalent(sigsmoothrestest[smoothrestest >= 5], 5)

  skip_if(skipall)
  expect_equivalent(smoothres, smoothrestest)
  expect_equivalent(sigsmoothres, sigsmoothrestest)
})
