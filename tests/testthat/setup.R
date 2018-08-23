# List files that are present before the start of testing
if (!exists("okfiles")) {
  okfiles <- list.files(here::here("tests/"), ".Rdata", full.names = T)
}
`%>%` <- magrittr::`%>%`

testthat::setup({
  # Download data if it is not present
  if (!dir.exists(here::here("tests/Bullet1")) |
    !dir.exists(here::here("tests/Bullet2"))) {
    dir.create(here::here("tests/Bullet1"))
    dir.create(here::here("tests/Bullet2"))
  }
  if (!file.exists(here::here("tests/Bullet1/Hamby252_Barrel1_Bullet1_Land2.x3p"))) {
    download.file(hamby252demo[[1]][2],
      destfile = here::here("tests/Bullet1/Hamby252_Barrel1_Bullet1_Land2.x3p"), quiet = T
    )
  }
  if (!file.exists(here::here("tests/Bullet1/Hamby252_Barrel1_Bullet2_Land4.x3p"))) {
    download.file(hamby252demo[[2]][4],
      destfile = here::here("tests/Bullet2/Hamby252_Barrel1_Bullet2_Land4.x3p"), quiet = T
    )
  }
})

# testthat::teardown({
#   file.remove(here::here("tests/Bullet1/Hamby252_Barrel1_Bullet1_Land2.x3p"))
#   unlink(here::here("tests/Bullet1"), recursive = T)
#   file.remove(here::here("tests/Bullet2/Hamby252_Barrel1_Bullet2_Land4.x3p"))
#   unlink(here::here("tests/Bullet2"), recursive = T)
# })

# test_read.R
# test_grooves.R
# test_signatures.R
if (!file.exists(here::here("tests/bullet1_only.Rdata"))) {
  message("Generating data file for bullet 1 land 2 with crosscuts, grooves, and signatures")
  b1_l2_x3p <- read_bullet(here::here("tests/Bullet1"), "x3p") %>%
    # dplyr::filter(dplyr::row_number() == 3) %>%
    # turn the scans such that (0,0) is bottom left
    dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) x %>%
          x3ptools::rotate_x3p(angle = -90) %>%
          x3ptools::y_flip_x3p())
    ) %>%
    dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) {
        # make sure all measurements are in microns
        x$surface.matrix <- x$surface.matrix * 10^6
        x$header.info$incrementY <- x$header.info$incrementY * 10^6
        x$header.info$incrementX <- x$header.info$incrementX * 10^6
        x
      })
    ) %>%
    dplyr::mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
    dplyr::mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
    dplyr::mutate(
      loess = purrr::map(ccdata, cc_fit_loess, span = .75),
      gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600)
    ) %>%
    dplyr::mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
    dplyr::mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves,
      method = "middle",
      return_plot = T
    )) %>%
    dplyr::mutate(grooves_quad = purrr::map(ccdata, cc_locate_grooves,
      method = "quadratic", return_plot = F
    )) %>%
    dplyr::mutate(
      sigs = purrr::map2(
        .x = ccdata, .y = grooves,
        .f = function(x, y) {
          cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
        }
      )
    )

  save(b1_l2_x3p, file = here::here("tests/bullet1_only.Rdata"))
}

if (!file.exists(here::here("tests/bullet1_crosscut_extra.Rdata"))) {
  load(here::here("tests/bullet1_only.Rdata"))

  b1_l2 <- b1_l2_x3p$x3p[[1]]
  b1_l2_df <- x3ptools::x3p_to_df(b1_l2)
  cc1 <- bulletxtrctr:::land_cc(50, b1_l2_df)
  save(b1_l2, b1_l2_df, cc1, file = here::here("tests/bullet1_crosscut_extra.Rdata"))
}

if (!file.exists(here::here("tests/bullets_signatures.Rdata"))) {
  message("Generating data file for bullet 1 land 2 and bullet 2 land 4 with crosscut, ccdata, grooves, and sigs.")
  load(here::here("tests/bullet1_only.Rdata"))

  b2_l4_x3p <- read_bullet(here::here("tests/Bullet2"), "x3p") %>%
    # dplyr::filter(dplyr::row_number() == 5) %>%
    # turn the scans such that (0,0) is bottom left
    dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) x %>%
          x3ptools::rotate_x3p(angle = -90) %>%
          x3ptools::y_flip_x3p())
    ) %>%
    dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) {
        # make sure all measurements are in microns
        x$surface.matrix <- x$surface.matrix * 10^6
        x$header.info$incrementY <- x$header.info$incrementY * 10^6
        x$header.info$incrementX <- x$header.info$incrementX * 10^6
        x
      })
    ) %>%
    dplyr::mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
    dplyr::mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
    dplyr::mutate(
      loess = purrr::map(ccdata, cc_fit_loess, span = .75),
      gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600)
    ) %>%
    dplyr::mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
    dplyr::mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves,
      method = "middle",
      return_plot = T
    )) %>%
    dplyr::mutate(
      sigs = purrr::map2(
        .x = ccdata, .y = grooves,
        .f = function(x, y) {
          cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
        }
      )
    )

  save(b1_l2_x3p, b2_l4_x3p, file = here::here("tests/bullets_signatures.Rdata"))
}

# test_align.R
# test_cms.R
# test_features.R
if (!file.exists(here::here("tests/bullets_match.Rdata"))) {
  message("Generating align.R data file for testing correctness.")
  load(here::here("tests/bullets_signatures.Rdata"))
  alignment <- sig_align(
    b1_l2_x3p$sigs[[1]]$sig,
    b2_l4_x3p$sigs[[1]]$sig
  )
  peaks <- list(
    sig1 = sig_get_peaks(alignment$bullets$sig1),
    sig2 = sig_get_peaks(alignment$bullets$sig2)
  )
  matches <- bulletxtrctr:::striation_identify_matches(peaks$sig1$lines, peaks$sig2$lines)
  maxcms <- sig_cms_max(alignment)
  features_legacy <- extract_features_all_legacy(maxcms)
  features <- extract_features_all(maxcms)
  match <- list(
    alignment = alignment, peaks = peaks, matches = matches,
    maxcms = maxcms, features_legacy = features_legacy,
    features = features
  )
  save(match, file = here::here("tests/bullets_match.Rdata"))
}
if (!file.exists(here::here("tests/single_features.Rdata"))) {
  features_single <- data.frame(
    rightcms = extract_feature_right_cms(striae = match$maxcms$lines),
    leftcms = extract_feature_left_cms(striae = match$maxcms$lines),
    cms2 = extract_feature_cms2(striae = match$maxcms$lines),
    cms = extract_feature_cms(striae = match$maxcms$lines),
    noncms = extract_feature_non_cms(striae = match$maxcms$lines),
    matches = extract_feature_matches(striae = match$maxcms$lines),
    mismatches = extract_feature_mismatches(striae = match$maxcms$lines),
    ccf = extract_feature_ccf(match$alignment$bullets)
  )
  save(features_single, file = here::here("tests/single_features.Rdata"))
}


# test_smooth.R
if (!file.exists(here::here("tests/smooth.Rdata"))) {
  message("Generating smooth.R data file for testing correctness")
  set.seed(3240583)
  tmp <- dplyr::data_frame(
    x = seq(-sqrt(5), sqrt(5), .03) %>% jitter(),
    y = rnorm(length(x), x^2, .1)
  )

  smoothres <- smoothloess(tmp$y, .5)
  sigsmoothres <- raw_sig_smooth(tmp$y, .5, c(-5, 5))
  save(sigsmoothres, smoothres, file = here::here("tests/smooth.Rdata"))
}
