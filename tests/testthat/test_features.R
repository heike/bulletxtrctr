context("features")
skipall <- T
if (requireNamespace("here") & requireNamespace("purrr")) {
  skipall <- F
  load(here::here("tests/bullets_match.Rdata"))

  featurestest_legacy <- extract_features_all_legacy(match$maxcms,
                                                     resolution = 1.5625)

  classes <- lapply(featurestest_legacy, mode) %>% unlist() %>% unique()

  featurestest_full <- extract_features_all(
    aligned = match$alignment,
    striae = match$maxcms,
    resolution = 1.5625
  )
}

test_that("features works as expected", {
  skip_if(skipall)
  expect_s3_class(featurestest_legacy, "data.frame")
  expect_equal(classes, "numeric")
  expect_equal(featurestest_legacy, match$features_legacy)
})

test_that("extract_feature_right_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) > 11) %>%
      extract_feature_right_cms(),
    0
  )
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) > 4) %>%
      extract_feature_right_cms(),
    6
  )
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) > 0) %>%
      extract_feature_right_cms(),
    10
  )
})

test_that("extract_feature_left_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 11) %>%
      extract_feature_left_cms(),
    10
  )
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 5) %>%
      extract_feature_left_cms(),
    4
  )
  expect_equal(
    data.frame(xmin = 1:10, match = (1:10) < 0) %>%
      extract_feature_left_cms(),
    0
  )
})


test_that("extract_feature_cms2 works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms2(),
    5
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 5,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_cms2(),
    2
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 0,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_cms2(),
    0
  )
})

test_that("extract_feature_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_cms(),
    10
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 5,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_cms(),
    4
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 0,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_cms(),
    0
  )
})

test_that("extract_feature_non_cms works as expected", {
  expect_equal(
    data.frame(xmin = 1:10, match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_non_cms(),
    0
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 5,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_non_cms(),
    6
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 0,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_non_cms(),
    10
  )
})

test_that("extract_helper_feature_n_striae works as expected", {
  expect_equal(
    data.frame(match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_helper_feature_n_striae(type = "peak", match = T),
    5
  )
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_helper_feature_n_striae(type = "peak", match = T),
    3
  )
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_helper_feature_n_striae(type = "valley", match = T),
    4
  )
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_helper_feature_n_striae(type = "peak", match = F),
    3
  )
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_helper_feature_n_striae(type = "valley", match = F),
    3
  )
})

test_that("extract_feature_matches works as expected", {
  expect_equal(
    data.frame(match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_matches(),
    10
  )
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_matches(),
    7
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 0,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_matches(),
    0
  )
})

test_that("extract_feature_mismatches works as expected", {
  expect_equal(
    data.frame(match = TRUE, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_mismatches(),
    0
  )
  expect_equal(
    data.frame(match = (1:10) <= 7, type = rep(c(-1, 1), times = 5)) %>%
      extract_feature_mismatches(),
    3
  )
  expect_equal(
    data.frame(
      xmin = 1:10, match = (1:10) < 0,
      type = rep(c(-1, 1), times = 5)
    ) %>%
      extract_feature_mismatches(),
    10
  )
})

test_that("extract_feature_sum_peaks works as expected", {
  expect_equal(
    data.frame(
      match = c(T, F, T, T, F, T),
      heights = c(1, -1, 1, -1, 1, -1)
    ) %>%
      extract_feature_sum_peaks(),
    4
  )
})

test_that("extract_feature_ccf works as expected", {
  expect_equal(extract_feature_ccf(
    data.frame(x = 1:10, sig1 = seq(0, .9, .1), sig2 = seq(.1, 1, .1))
  ), 1)
  expect_gte(
    extract_feature_ccf(
      data.frame(
        x = 1:10, sig1 = seq(0, sqrt(.9), length.out = 10)^2,
        sig2 = seq(.1, 1, .1)
      )
    ), .96269
  )
  expect_equivalent(
    extract_feature_ccf(
      data.frame(
        x = 1:10, sig1 = c(NA, NA, seq(.3, 1, .1)),
        sig2 = seq(0, .9, .1), sig3 = c(NA, seq(.2, 1, .1))
      )
    ) %>%
      sum(na.rm = T),
    9
  )
})


test_that("extract_feature_rough_cor works as expected", {
  expect_error(extract_feature_rough_cor(
    data.frame(x = 1:10, sig1 = seq(0, .9, .1), sig2 = seq(.1, 1, .1))
  ), "length.*y.* not greater than 10")
  expect_lte(
    extract_feature_rough_cor(
      data.frame(
        x = 1:20, sig1 = seq(0, sqrt(.9), length.out = 20)^2,
        sig2 = seq(.05, 1, .05)
      )
    ), -1
  )
  expect_gte(
    extract_feature_rough_cor(
      data.frame(
        x = 1:20, sig1 = c(NA, NA, seq(.15, 1, .05)),
        sig2 = seq(0, .95, .05), sig3 = c(NA, seq(.1, 1, .05))
      )
    ) %>%
      sum(na.rm = T),
    8.59
  )
})

test_that("extract_feature_lag works as expected", {
  expect_equivalent(
    extract_feature_lag(
      data.frame(
        x = 1:10, sig1 = seq(0, .9, .1),
        sig2 = c(NA, NA, seq(.3, 1, .1))
      )
    ),
    2
  )
  expect_equivalent(
    extract_feature_lag(
      data.frame(
        x = 1:10, sig1 = c(NA, NA, seq(.3, 1, .1)),
        sig2 = seq(0, .9, .1)
      )
    ),
    -2
  )
  expect_equivalent(
    extract_feature_lag(
      data.frame(
        x = 1:10, sig1 = c(NA, NA, seq(.3, 1, .1)),
        sig2 = seq(0, .9, .1), sig3 = c(NA, seq(.2, 1, .1))
      )
    ),
    c(2, 0, 1)
  )
})

test_that("extract_feature_D works as expected", {
  expect_equivalent(
    extract_feature_D(
      data.frame(
        x = 1:10, sig1 = seq(0, .9, .1),
        sig2 = c(NA, NA, seq(.2, .9, .1))
      )
    ),
    0
  )
  expect_gte(
    extract_feature_D(
      data.frame(
        x = 1:10, sig1 = c(NA, NA, seq(.3, 1, .1)),
        sig2 = seq(0, .9, .1)
      )
    ),
    0.0395
  )
  expect_gte(
    extract_feature_D(
      data.frame(
        x = 1:10, sig1 = c(NA, NA, seq(.3, 1, .1)),
        sig2 = seq(0, .9, .1), sig3 = c(NA, seq(.2, 1, .1))
      )
    )[3],
    0.0351
  )
})

test_that("extract_feature_length works as expected", {
  expect_equivalent(
    extract_feature_length(
      data.frame(
        x = 1:10, sig1 = seq(0, .9, .1),
        sig2 = c(NA, NA, seq(.2, .9, .1))
      )
    ),
    8
  )
})

test_that("extract_feature_overlap works as expected", {
  expect_equivalent(
    extract_feature_overlap(
      data.frame(
        x = 1:10, sig1 = seq(0, .9, .1),
        sig2 = c(NA, NA, seq(.2, .9, .1))
      )
    ),
    1
  )
})

test_that("extract_features_all works as expected", {
  skip_if(skipall)
  expect_equivalent(match$features, featurestest_full)
})
