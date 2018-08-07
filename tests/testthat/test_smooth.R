context("smooth")

set.seed(3240583)
tmp <- data_frame(
  x = seq(-sqrt(5), sqrt(5), .03) %>% jitter(),
  y = rnorm(length(x), x^2, .1)
)

# smoothres <- smoothloess(tmp$y, .5)
# sigsmoothres <- raw_sig_smooth(tmp$y, .5, c(-5, 5))
# save(sigsmoothres, smoothres, file = "../../tests/testdata/correct_test_smooth.Rdata")

load(here::here("tests/testdata/correct_test_smooth.Rdata"))

smoothrestest <- smoothloess(tmp$y, .5)
sigsmoothrestest <- raw_sig_smooth(tmp$y, .5, c(-5, 5))

test_that("smooth functions works as expected", {
  expect_is(smoothrestest, "numeric")
  expect_is(sigsmoothrestest, "numeric")

  expect_equivalent(smoothrestest[smoothrestest<5],
                    sigsmoothrestest[smoothrestest<5])
  expect_equivalent(sigsmoothrestest[smoothrestest>=5], 5)

  expect_equal(smoothres, smoothrestest)
  expect_equal(sigsmoothres, sigsmoothrestest)

})
