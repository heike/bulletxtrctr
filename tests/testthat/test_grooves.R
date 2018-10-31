context("grooves")

skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/bullet1_only.Rdata"))
  testb1 <- b1_l2_x3p %>%
    dplyr::select(-grooves, -grooves_mid) %>%
    dplyr::mutate(
      grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T),
      grooves_mid = purrr::map(ccdata, cc_locate_grooves,
                               method = "middle", return_plot = T),
      grooves_quad = purrr::map(ccdata, cc_locate_grooves,
                                method = "quadratic", return_plot = F),
      grooves_log = purrr::map(ccdata, cc_locate_grooves,
                               method = "logisticlegacy", return_plot = F),
      grooves_lassofull = purrr::map(ccdata, cc_locate_grooves,
                                     method = "lassofull", return_plot = F),
      grooves_lassobasic = purrr::map(ccdata, cc_locate_grooves,
                                      method = "lassobasic", return_plot = F))
}

# ccdata with no grooves - perfect parabola
flat_ccdata <- data.frame(x = seq(0, 1000, 1.5625), y = 100) %>%
  dplyr::mutate(value = 50)

test_that("grooves works as expected", {
  # Tests that don't require previous data
  expect_silent(cc_locate_grooves(flat_ccdata, method = "rollapply"))

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
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "quadratic",
                            return_plot = T)
  expect_length(tmp2, 2)
  expect_equal(names(tmp2), c("groove", "plot"))
  expect_s3_class(tmp2$plot, "ggplot")

  ## Logistic
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "logisticlegacy",
                            return_plot = T)
  expect_length(tmp2, 2)
  expect_equal(names(tmp2), c("groove", "plot"))
  expect_s3_class(tmp2$plot, "ggplot")

  ## Lasso - Full
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "lassofull",
                            return_plot = T)
  expect_length(tmp2, 2)
  expect_equal(names(tmp2), c("groove", "plot"))
  expect_s3_class(tmp2$plot, "ggplot")

  ## Lasso - Basic
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "lassobasic",
                            return_plot = T)
  expect_length(tmp2, 2)
  expect_equal(names(tmp2), c("groove", "plot"))
  expect_s3_class(tmp2$plot, "ggplot")

  # Test that plots aren't generated when return_plot is left to the default value (F)
  ## Rollapply
  expect_length(tmp, 1)
  expect_length(tmp$groove, 2)
  expect_is(tmp$groove, "numeric")

  ## Middle
  tmp2 <- cc_locate_grooves(testb1$ccdata[[1]], method = "middle",
                            return_plot = F)
  expect_length(tmp2, 1)
  expect_length(tmp2$groove, 2)
  expect_is(tmp2$groove, "numeric")

  ## Quadratic
  expect_length(testb1$grooves_quad[[1]], 1)
  expect_length(testb1$grooves_quad[[1]]$groove, 2)
  expect_is(testb1$grooves_quad[[1]]$groove, "numeric")

  ## Logistic
  expect_length(testb1$grooves_log[[1]], 1)
  expect_length(testb1$grooves_log[[1]]$groove, 2)
  expect_is(testb1$grooves_log[[1]]$groove, "numeric")

  ## Lasso - full
  expect_length(testb1$grooves_lassofull[[1]], 1)
  expect_length(testb1$grooves_lassofull[[1]]$groove, 2)
  expect_is(testb1$grooves_lassofull[[1]]$groove, "numeric")

  ## Lasso - basic
  expect_length(testb1$grooves_lassobasic[[1]], 1)
  expect_length(testb1$grooves_lassobasic[[1]]$groove, 2)
  expect_is(testb1$grooves_lassobasic[[1]]$groove, "numeric")


  # Test other conditions
  ## Middle - middle argument trims things
  expect_equal(
    names(cc_locate_grooves(testb1$ccdata[[1]], "middle", middle = 50)$groove),
    c("25%", "75%")
  )
  ## Rollapply - multiple y values
  expect_message(
    cc_locate_grooves(rbind(testb1$ccdata[[1]], testb1$ccdata[[1]] %>%
                              dplyr::mutate(y = 103))),
    "summarizing \\d{1,} profiles by averaging across values"
  )

  ## Rollapply - mean left and mean right
  tmp3 <- cc_locate_grooves(testb1$ccdata[[1]],
    method = "rollapply",
    mean_left = 200, mean_right = 2000
  )
  expect_error(expect_equivalent(b1_l2_x3p$grooves[[1]]$groove, tmp3$groove))

  expect_silent(
    testb1$ccdata[[1]] %>%
      dplyr::mutate(value = rev(value)) %>%
      get_grooves_rollapply(
        x = .$x, value = .$value, smoothfactor = 15, adjust = 10,
        groove_cutoff = 400, second_smooth = F, return_plot = F
      )
  )
  expect_silent(
    testb1$ccdata[[1]] %>%
      get_grooves_rollapply(
        x = .$x, value = .$value, smoothfactor = 15, adjust = 10,
        groove_cutoff = 400, second_smooth = F, return_plot = F
      )
  )

  # Check numerically identical for groove locations, at least...
  expect_identical(b1_l2_x3p$grooves[[1]]$groove,
                   testb1$grooves[[1]]$groove)
  expect_identical(b1_l2_x3p$grooves_mid[[1]]$groove,
                   testb1$grooves_mid[[1]]$groove)
  expect_identical(b1_l2_x3p$grooves_quad[[1]]$groove,
                   testb1$grooves_quad[[1]]$groove)
  expect_identical(b1_l2_x3p$grooves_quad[[1]]$groove,
                   testb1$grooves_quad[[1]]$groove)
  expect_identical(b1_l2_x3p$grooves_log[[1]]$groove,
                   testb1$grooves_log[[1]]$groove)
  expect_identical(b1_l2_x3p$grooves_lassofull[[1]]$groove,
                   testb1$grooves_lassofull[[1]]$groove)
  expect_identical(b1_l2_x3p$grooves_lassobasic[[1]]$groove,
                   testb1$grooves_lassobasic[[1]]$groove)
})

