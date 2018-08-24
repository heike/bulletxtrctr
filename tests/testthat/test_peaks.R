context("peaks")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F

  load(here::here("tests/bullets_match.Rdata"))

  peakstest <- sig_get_peaks(match$alignment$bullets$sig1)
  peakstest2 <- sig_get_peaks(match$alignment$bullets$sig2)
  matchestest <- striation_identify_matches(peakstest$lines, peakstest2$lines)
}

test_that("peaks works as expected", {
  skip_if(skipall)
  expect_equal(names(peakstest), c(
    "peaks", "valleys", "extrema", "peaks.heights",
    "valleys.heights", "lines", "plot", "dframe"
  ))
  expect_is(peakstest$peaks, "integer")
  expect_is(peakstest$valleys, "integer")
  expect_is(peakstest$extrema, "integer")
  expect_is(peakstest$peaks.heights, "numeric")
  expect_equal(length(peakstest$peaks), length(peakstest$peaks.heights))
  expect_equal(length(peakstest$valleys), length(peakstest$valleys.heights))
  expect_equal(
    length(peakstest$peaks) + length(peakstest$valleys),
    length(peakstest$extrema)
  )
  expect_equal(names(peakstest$lines), c("xmin", "xmax", "type", "extrema", "heights"))
  expect_equal(lapply(peakstest$lines, mode) %>% as.character(), rep("numeric", 5))
  expect_s3_class(peakstest$plot, "ggplot")
  expect_equal(names(peakstest$dframe), c("x", "smoothed"))
  expect_equal(lapply(peakstest$dframe, mode) %>% as.character(), c("numeric", "numeric"))
  expect_equal(match$peaks$sig1, peakstest)
})

test_that("striation_identify_matches works as expected", {
  expect_equal(names(matchestest), c("xmin", "xmax", "match", "type",
                                     "meany", "heights", "sdheights"))
  expect_is(matchestest$xmin, "numeric")
  expect_is(matchestest$xmax, "numeric")
  expect_is(matchestest$match, "logical")
  expect_is(matchestest$meany, "numeric")
  expect_is(matchestest$heights, "numeric")
  expect_is(matchestest$sdheights, "numeric")
  expect_equal(match$matches, matchestest)
})

