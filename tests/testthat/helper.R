`%>%` <- magrittr::`%>%`

# test_read.R
# test_grooves.R
# test_signatures.R
if (!file.exists(here::here("tests/bullet1_only.Rdata"))) {
  message("Generating data file for bullet 1 land 3 with crosscuts, grooves, and signatures")
  b1_l3_x3p <- read_bullet(here::here("data/Bullet1"), "x3p") %>%
    dplyr::filter(dplyr::row_number() == 3) %>%
    # turn the scans such that (0,0) is bottom left
    dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) x %>%
                                 x3ptools::rotate_x3p(angle=-90) %>%
                                 x3ptools::y_flip_x3p())
    ) %>% dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) {
        # make sure all measurements are in microns
        x$surface.matrix <- x$surface.matrix*10^6
        x$header.info$incrementY <- x$header.info$incrementY*10^6
        x$header.info$incrementX <- x$header.info$incrementX*10^6
        x
      })
    ) %>%
    dplyr::mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
    dplyr::mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
    dplyr::mutate(loess = purrr::map(ccdata, cc_fit_loess, span = .75),
                  gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600)) %>%
    dplyr::mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
    dplyr::mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves, method = "middle",
                                           return_plot = T)) %>%
    dplyr::mutate(
      sigs = purrr::map2(
        .x = ccdata, .y = grooves,
        .f = function(x, y) {
          cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)})
    )

  b1_l3 <- b1_l3_x3p$x3p[[1]]
  b1_l3_df <- x3ptools::x3p_to_df(b1_l3)
  cc1 <- land_cc(50, b1_l3_df)
  save(b1_l3, b1_l3_df, cc1, file = here::here("tests/bullet1_crosscut_extra.Rdata"))

  save(b1_l3_x3p, file = here::here("tests/bullet1_only.Rdata"))
}

if (!file.exists(here::here("tests/bullets_signatures.Rdata"))) {
  message("Generating data file for bullet 1 land 3 and bullet 2 land 5 with crosscut, ccdata, grooves, and sigs.")
  load(here::here("tests/bullet1_only.Rdata"))

  b2_l5_x3p <- read_bullet(here::here("data/Bullet2"), "x3p") %>%
    dplyr::filter(dplyr::row_number() == 5) %>%
    # turn the scans such that (0,0) is bottom left
    dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) x %>%
                                 x3ptools::rotate_x3p(angle=-90) %>%
                                 x3ptools::y_flip_x3p())
    ) %>% dplyr::mutate(
      x3p = x3p %>% purrr::map(.f = function(x) {
        # make sure all measurements are in microns
        x$surface.matrix <- x$surface.matrix*10^6
        x$header.info$incrementY <- x$header.info$incrementY*10^6
        x$header.info$incrementX <- x$header.info$incrementX*10^6
        x
      })
    ) %>%
    dplyr::mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)) %>%
    dplyr::mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)) %>%
    dplyr::mutate(loess = purrr::map(ccdata, cc_fit_loess, span = .75),
                  gauss = purrr::map(ccdata, cc_fit_gaussian, span = 600)) %>%
    dplyr::mutate(grooves = purrr::map(ccdata, cc_locate_grooves, return_plot = T)) %>%
    dplyr::mutate(grooves_mid = purrr::map(ccdata, cc_locate_grooves, method = "middle",
                                           return_plot = T)) %>%
    dplyr::mutate(
      sigs = purrr::map2(
        .x = ccdata, .y = grooves,
        .f = function(x, y) {
          cc_get_signature(ccdata=x, grooves = y, span1 = 0.75, span2=0.03)})
    )

  save(b1_l3_x3p, b2_l5_x3p, file = here::here("tests/bullets_signatures.Rdata"))
}

# test_align.R
# test_cms.R
# test_features.R
if (!file.exists(here::here("tests/bullets_match.Rdata"))) {
  message("Generating align.R data file for testing correctness.")
  load(here::here("tests/bullets_signatures.Rdata"))
  alignment <- sig_align(b1_l3_x3p$sigs[[1]]$sig,
                        b2_l5_x3p$sigs[[1]]$sig)
  peaks <- list(sig1 = sig_get_peaks(alignment$bullets$sig1),
                sig2 = sig_get_peaks(alignment$bullets$sig2))
  matches <- striation_identify_matches(peaks$sig1$lines, peaks$sig2$lines)
  maxcms <- sig_cms_max(alignment)
  features <- extract_features_all(maxcms)
  match <- list(alignment = alignment, peaks = peaks, matches = matches,
                maxcms = maxcms, features = features)
  save(match, file = here::here("tests/bullets_match.Rdata"))
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
