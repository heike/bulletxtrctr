context("grooves")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/bullet1_only.Rdata"))
  testb1 <- b1_l3_x3p %>%
    dplyr::select(-grooves, -grooves_mid) %>%
    dplyr::mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
    dplyr::mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves, method = "middle", return_plot = T)) %>%
    dplyr::mutate(grooves_quad = purrr::map(ccdata, cc_locate_grooves, method = "quadratic", return_plot = F))

}

test_that("grooves works as expected", {
  skip_if(skipall)
  expect_silent(tmp <- cc_locate_grooves(testb1$ccdata[[1]]))
  expect_silent(cc_locate_grooves(testb1$ccdata[[1]], "middle"))

  # Test that plots are generated correctly when return_plot is T
  ## Rollapply
  expect_length(testb1$grooves[[1]], 2)
  expect_equal(names(testb1$grooves[[1]]), c("groove", "plot"))
  expect_s3_class(testb1$grooves[[1]]$plot, "ggplot")

  ## Middle
  expect_length(testb1$grooves_mid[[1]], 2)
  expect_equal(names(testb1$grooves_mid[[1]]), c("groove", "plot"))
  expect_s3_class(testb1$grooves_mid[[1]]$plot, "ggplot")

  ## Quadratic
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "quadratic", return_plot = T)
  expect_length(tmp2, 2)
  expect_equal(names(tmp2), c("groove", "plot"))
  expect_s3_class(tmp2$plot, "ggplot")

  # Test that plots aren't generated when return_plot is left to the default value (F)
  ## Rollapply
  expect_length(tmp, 1)
  expect_length(tmp$groove, 2)
  expect_is(tmp$groove, "numeric")

  ## Middle
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "middle", return_plot = F)
  expect_length(tmp2, 1)
  expect_length(tmp2$groove, 2)
  expect_is(tmp2$groove, "numeric")

  ## Quadratic
  expect_length(testb1$grooves_quad[[1]], 1)
  expect_length(testb1$grooves_quad[[1]]$groove, 2)
  expect_is(testb1$grooves_quad[[1]]$groove, "numeric")

  # Test other conditions
  ## Middle - middle argument trims things
  expect_equal(names(cc_locate_grooves(testb1$ccdata[[1]], "middle", middle = 50)$groove),
               c("25%", "75%"))
  ## Rollapply - multiple y values
  expect_message(cc_locate_grooves(rbind(testb1$ccdata[[1]], testb1$ccdata[[1]] %>% dplyr::mutate(y = 100))),
                 "summarizing \\d{1,} profiles by averaging across values")

  ## Rollapply - mean left and mean right
  tmp3 <- cc_locate_grooves(testb1$ccdata[[1]], method = "rollapply",
                            mean_left = 200, mean_right = 2000, second_smooth = F)
  expect_error(expect_equivalent(b1_l3_x3p$grooves[[1]]$groove, tmp3$groove))

  # Check numerically identical for groove locations, at least...
  expect_identical(b1_l3_x3p$grooves[[1]]$groove, testb1$grooves[[1]]$groove)
  expect_identical(b1_l3_x3p$grooves_mid[[1]]$groove, testb1$grooves_mid[[1]]$groove)
  expect_identical(b1_l3_x3p$grooves_quad[[1]]$groove, testb1$grooves_quad[[1]]$groove)
})
