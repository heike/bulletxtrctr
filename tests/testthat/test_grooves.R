context("grooves")

load(here::here("tests/bullet1_only.Rdata"))
testb1 <- b1_l3_x3p %>%
  dplyr::select(-grooves, -grooves_mid) %>%
  dplyr::mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
  dplyr::mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves, method = "middle", return_plot = T))

test_that("grooves works as expected", {
  expect_silent(tmp <- cc_locate_grooves(testb1$ccdata[[1]]))
  expect_silent(cc_locate_grooves(testb1$ccdata[[1]], "middle"))

  # Test that plots are generated correctly when return_plot is left to the default value (F)
  expect_length(testb1$grooves[[1]], 2)
  expect_equal(names(testb1$grooves[[1]]), c("groove", "plot"))
  expect_s3_class(testb1$grooves[[1]]$plot, "ggplot")

  expect_length(testb1$grooves_mid[[1]], 2)
  expect_equal(names(testb1$grooves_mid[[1]]), c("groove", "plot"))
  expect_s3_class(testb1$grooves_mid[[1]]$plot, "ggplot")

  # Check numerically identical for groove locations, at least...
  expect_identical(b1_l3_x3p$grooves[[1]]$groove, testb1$grooves[[1]]$groove)
  expect_identical(b1_l3_x3p$grooves_mid[[1]]$groove, testb1$grooves_mid[[1]]$groove)

  # Test that plots aren't generated when return_plot is left to the default value (F)
  expect_length(tmp, 1)
  expect_length(tmp$groove, 2)
  expect_is(tmp$groove, "numeric")

  ## What other groove methods should be tested?
})
