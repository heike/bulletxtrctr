context("crosscut")
skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F

  load(here::here("tests/bullet1_only.Rdata"))
  load(here::here("tests/bullet1_crosscut_extra.Rdata"))

  testb1 <- b1_l3_x3p %>%
    dplyr::select(-crosscut, -ccdata)
  testb1_l3 <- testb1$x3p[[1]]
  testb1_l3_df <- x3ptools::x3p_to_df(testb1_l3)
  testcc1 <- land_cc(50, testb1_l3_df)
  testb1 <- testb1 %>%
    dplyr::mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
    dplyr::mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut))

}

test_that("switch_xy works as expected", {
  expect_equal(switch_xy(data.frame(x = 1:3, y = 5:7)), data.frame(y = 1:3, x = 5:7))
})

test_that("land_cc works as expected", {
  skip_if(skipall)
  expect_s3_class(testcc1, "data.frame")
  expect_equal(names(testcc1), c("x", "y", "value", "fitted", "raw_sig", "se", "abs_resid", "chop",  "resid"))
  expect_equivalent(testcc1, cc1)
})

test_that("x3p_crosscut_optimize works as expected", {
  skip_if(skipall)
  expect_silent(x3p_crosscut_optimize(b1_l3))
  expect_silent(suppressWarnings(x3p_crosscut_optimize(hamby252demo$bullet1[3])))
  expect_gte(testb1$crosscut, 50)
  expect(is.numeric(testb1$crosscut))
  expect_equivalent(testb1$crosscut, b1_l3_x3p$crosscut)
})

test_that("x3p_crosscut works as expected", {
  skip_if(skipall)
  expect_silent(x3p_crosscut(testb1_l3, testb1$crosscut))
  expect_silent(suppressWarnings(x3p_crosscut(hamby252demo$bullet1[3])))
  expect_s3_class(testb1$ccdata[[1]], "data.frame")
  expect_equal(names(testb1$ccdata[[1]]), c("x", "y", "value"))
  expect_length(attr(testb1$ccdata[[1]], "header.info"), 4)
  expect_equal(sort(names(attr(testb1$ccdata[[1]], "header.info"))), sort(c("sizeY", "sizeX", "incrementY", "incrementX")))
  expect_equivalent(testb1$ccdata[[1]], b1_l3_x3p$ccdata[[1]])
})

