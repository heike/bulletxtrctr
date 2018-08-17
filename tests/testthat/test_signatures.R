context("signatures")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/bullet1_only.Rdata"))

  suppressWarnings({
    testb1 <- b1_l2_x3p %>%
      dplyr::select(-sigs) %>%
      dplyr::mutate(
        sigsLL = purrr::map2(
          .x = ccdata, .y = grooves,
          .f = function(x, y) {
            cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)}),
        sigsLS = purrr::map2(
          .x = ccdata, .y = grooves,
          .f = function(x, y) {
            cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.01)}),
        sigsSL = purrr::map2(
          .x = ccdata, .y = grooves,
          .f = function(x, y) {
            cc_get_signature(ccdata = x, grooves = y, span1 = 0.25, span2 = 0.03)}),
        sigsSS = purrr::map2(
          .x = ccdata, .y = grooves,
          .f = function(x, y) {
            cc_get_signature(ccdata = x, grooves = y, span1 = 0.25, span2 = 0.01)})
      )
  })
}


test_that("signatures works as expected", {
  skip_if(skipall)
  expect_s3_class(testb1$sigsLL[[1]], "data.frame")
  expect_equivalent(names(testb1$sigsLL[[1]]), c("x", "y", "value", "raw_sig", "se", "sig"))
  expect_type(testb1$sigsLL[[1]]$x, "double")
  expect_type(testb1$sigsLL[[1]]$y, "double")
  expect_type(testb1$sigsLL[[1]]$value, "double")
  expect_type(testb1$sigsLL[[1]]$raw_sig, "double")
  expect_type(testb1$sigsLL[[1]]$se, "double")
  expect_type(testb1$sigsLL[[1]]$sig, "double")
  expect_length(unique(testb1$sigsLL[[1]]$y), 1)
})

test_that("signatures is numerically correct", {
  skip_if(skipall)
  expect_equivalent(b1_l2_x3p$sigs, testb1$sigsLL)
  expect_error(expect_equivalent(testb1$sigsLL[[1]]$sig, testb1$sigsLS[[1]]$sig))
  expect_error(expect_equivalent(testb1$sigsLL[[1]]$sig, testb1$sigsSL[[1]]$sig))
  expect_error(expect_equivalent(testb1$sigsLL[[1]]$sig, testb1$sigsSS[[1]]$sig))
})

