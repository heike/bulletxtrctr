context("cms")
skipall <- T
if (requireNamespace("here")) {
  skipall <- F
  load(here::here("tests/bullets_match.Rdata"))
  maxcmstest <- sig_cms_max(match$alignment)
}

x <- c(rep(0, 5), rep(1, 3), 0, rep(1, 6))
y <- as.table(c("3" = 1, "6" = 1))
dimnames(y) <- list(c("3", "6"))

test_that("get_runs works as expected", {
  expect_warning(get_runs(x), "Converting .* to a logical vector")
  expect_equivalent(get_runs(x == 1), y)
  expect_equal(get_longest_run(x == 1), 6)
})

test_that("sig_cms_max works", {
  skip_if(skipall)
  expect_equal(names(maxcmstest), c("maxCMS", "ccf", "lag", "lines", "lands"))
  expect_equal(names(maxcmstest$lines), c(
    "xmin", "xmax", "match", "type",
    "meany", "heights", "sdheights"
  ))
  expect_equal(names(maxcmstest$lands), c("x", "sig1", "sig2"))
  expect_is(maxcmstest$maxCMS, "numeric")
  expect_is(maxcmstest$ccf, "numeric")
  expect_is(maxcmstest$lag, "numeric")
  expect_s3_class(maxcmstest$lines, "data.frame")
  expect_s3_class(maxcmstest$lands, "data.frame")
  expect_is(maxcmstest$lines$xmin, "numeric")
  expect_is(maxcmstest$lines$xmax, "numeric")
  expect_is(maxcmstest$lines$match, "logical")
  expect_is(maxcmstest$lines$type, "numeric")
  expect_is(maxcmstest$lines$meany, "numeric")
  expect_is(maxcmstest$lines$heights, "numeric")
  expect_is(maxcmstest$lines$sdheights, "numeric")
  expect_is(maxcmstest$lands$x, "integer")
  expect_is(maxcmstest$lands$sig1, "numeric")
  expect_is(maxcmstest$lands$sig2, "numeric")
  expect_equal(maxcmstest, match$maxcms)
  expect_equal(maxcmstest$maxCMS, match$maxcms$maxCMS)
  expect_equal(maxcmstest$ccf, match$maxcms$ccf)
  expect_equal(maxcmstest$lag, match$maxcms$lag)
  expect_equal(maxcmstest$lines, match$maxcms$lines)
  expect_equal(maxcmstest$lands, match$maxcms$lands)
})
