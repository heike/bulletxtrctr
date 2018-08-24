context("loess")
skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F

  load(here::here("tests/bullet1_only.Rdata"))
  testb1 <- b1_l2_x3p %>%
    dplyr::select(ccdata) %>%
    dplyr::mutate(
      loess = purrr::map(ccdata, cc_fit_loess, span = .75),
      loess2 = purrr::map(ccdata, cc_fit_loess, span = .25),
      gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600),
      gauss2 = purrr::map(ccdata, cc_fit_gaussian, span = 300)
    )

  ex_names <- c("x", "y", "value", "fitted", "raw_sig", "se", "abs_resid", "chop")
}

test_that("loess works as expected", {
  skip_if(skipall)
  expect_s3_class(testb1$loess[[1]], "data.frame")
  expect_equivalent(names(testb1$loess[[1]]), ex_names)
  expect_equivalent(
    summarize_all(testb1$loess[[1]], mode) %>% as.character(),
    c(rep("numeric", 7), "logical")
  )
  expect_equivalent(b1_l2_x3p$loess, testb1$loess)
  expect_equivalent(b1_l2_x3p$gauss, testb1$gauss)
  expect_error(expect_equivalent(testb1$loess, testb1$loess2))
  expect_error(expect_equivalent(testb1$gauss, testb1$gauss2))
})
