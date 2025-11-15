# Package index

## All functions

- [`F_T()`](https://heike.github.io/bulletxtrctr/reference/F_T.md)
  [`f_T()`](https://heike.github.io/bulletxtrctr/reference/F_T.md) :

  \#' Extract results from test in tidy form \#' \#' `tidy.phase.test`
  expands the tidy method for test.phase objects. It gives a summary of
  the relevant \#' parameters and estimates. \#' @param x phase.test
  object as returned from `phase_test` \#' @param ... ignored \#'
  @export \#' @importFrom broom tidy \#' @examples \#' logo \<-
  x3ptools::x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
  \#' print(logo) tidy \<- function (x, ...) with(x, tibble(estimate,
  estimate1, estimate2, statistic, p.value, parameter)) Reference
  distribution and debsity for the test statistic between Same-source
  and Different-source averages using phase selection

- [`bootstrap_k()`](https://heike.github.io/bulletxtrctr/reference/bootstrap_k.md)
  : Helper function: bootstrap scores

- [`br411`](https://heike.github.io/bulletxtrctr/reference/br411.md) :
  3d topological surface measurements for one land of a bullet from the
  Hamby study

- [`bullet_pipeline()`](https://heike.github.io/bulletxtrctr/reference/bullet_pipeline.md)
  : Helper file to setup data

- [`bullet_to_land_predict()`](https://heike.github.io/bulletxtrctr/reference/bullet_to_land_predict.md)
  : Get land to land prediction based on bullet to bullet comparisons

- [`cc_fit_gaussian()`](https://heike.github.io/bulletxtrctr/reference/cc_fit_gaussian.md)
  : Use a gaussian filter on bullet data frame

- [`cc_fit_loess()`](https://heike.github.io/bulletxtrctr/reference/cc_fit_loess.md)
  : Fit a loess curve to a bullet data frame

- [`cc_get_signature()`](https://heike.github.io/bulletxtrctr/reference/cc_get_signature.md)
  : Extract signature from crosscut

- [`cc_locate_grooves()`](https://heike.github.io/bulletxtrctr/reference/cc_locate_grooves.md)
  : Find the grooves of a bullet land

- [`check_align()`](https://heike.github.io/bulletxtrctr/reference/check_align.md)
  : Check align output

- [`check_ccdata()`](https://heike.github.io/bulletxtrctr/reference/check_ccdata.md)
  : Check object returned by x3p_crosscut_optimize

- [`check_grooves()`](https://heike.github.io/bulletxtrctr/reference/check_grooves.md)
  : Check grooves for correctness

- [`check_loess_fit()`](https://heike.github.io/bulletxtrctr/reference/check_loess_fit.md)
  : Check loess or gaussian curve fit object

- [`check_sig()`](https://heike.github.io/bulletxtrctr/reference/check_sig.md)
  : Check signature object

- [`check_striae()`](https://heike.github.io/bulletxtrctr/reference/check_striae.md)
  : Check a striae object is valid

- [`check_x3p()`](https://heike.github.io/bulletxtrctr/reference/check_x3p.md)
  : Check whether an x3p argument is character or filename, return an
  x3p object

- [`compute_average_scores()`](https://heike.github.io/bulletxtrctr/reference/compute_average_scores.md)
  : Get average scores for bullet to bullet comparisons

- [`extract_feature_D()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_D.md)
  : Extract average distance between two (or more) aligned signatures

- [`extract_feature_ccf()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_ccf.md)
  : Extract ccf from two (or more) aligned signatures

- [`extract_feature_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms.md)
  : Extract number of consecutively matching striation marks (peaks and
  valleys) from two aligned signatures

- [`extract_feature_cms2()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms2.md)
  : Extract number of consecutively matching elevated striation marks
  from two aligned signatures

- [`extract_feature_cms2_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms2_per_mm.md)
  : Extract scaled number of consecutively matching elevated striation
  marks from two aligned signatures

- [`extract_feature_cms_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms_per_mm.md)
  : Extract scaled number of consecutively matching striation marks
  (peaks and valleys) from two aligned signatures

- [`extract_feature_lag()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_lag.md)
  : Extract lag from two (or more) aligned signatures

- [`extract_feature_lag_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_lag_mm.md)
  : Extract lag in mm from two (or more) aligned signatures

- [`extract_feature_left_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_left_cms.md)
  : Extract number of consecutively matching elevated striation marks
  from the left of two aligned signatures

- [`extract_feature_length()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length.md)
  : Extract length of two (aligned) signatures

- [`extract_feature_length_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length_mm.md)
  : Length of two (aligned) signatures in millimeter

- [`extract_feature_matches()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_matches.md)
  : Extract number of matching striation marks from two aligned
  signatures

- [`extract_feature_matches_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_matches_per_mm.md)
  : Extract scaled number of matching striation marks from two aligned
  signatures

- [`extract_feature_mismatches()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_mismatches.md)
  : Extract number of mismatched striation marks from two aligned
  signatures

- [`extract_feature_mismatches_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_mismatches_per_mm.md)
  : Extract number of mismatched striation marks from two aligned
  signatures

- [`extract_feature_non_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_non_cms.md)
  : Extract number of consecutively non-matching striation marks from
  two aligned signatures

- [`extract_feature_non_cms_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_non_cms_per_mm.md)
  : Extract scaled number of consecutively non-matching striation marks
  from two aligned signatures

- [`extract_feature_overlap()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_overlap.md)
  : Extract overlap between two aligned signatures

- [`extract_feature_right_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_right_cms.md)
  : Extract number of consecutively matching elevated striation marks
  from the right of two aligned signatures

- [`extract_feature_rough_cor()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_rough_cor.md)
  : Extract rough correlation from two (or more) aligned signatures

- [`extract_feature_sd_D()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_sd_D.md)
  : Extract variation in the height measurements between two aligned
  signatures

- [`extract_feature_sum_peaks()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_sum_peaks.md)
  : Extract the combined height of aligned striae between two aligned
  signatures

- [`extract_features_all()`](https://heike.github.io/bulletxtrctr/reference/extract_features_all.md)
  : Extract features from aligned signatures

- [`extract_features_all_legacy()`](https://heike.github.io/bulletxtrctr/reference/extract_features_all_legacy.md)
  : Extract features from aligned signatures (legacy)

- [`extract_helper_feature_n_striae()`](https://heike.github.io/bulletxtrctr/reference/extract_helper_feature_n_striae.md)
  : Extract information for striation marks from two aligned signatures

- [`get_ccf()`](https://heike.github.io/bulletxtrctr/reference/get_ccf.md)
  : Cross correlation function between two vectors

- [`get_longest_run()`](https://heike.github.io/bulletxtrctr/reference/get_longest_run.md)
  : Length of the longest run of TRUEs

- [`get_phases()`](https://heike.github.io/bulletxtrctr/reference/get_phases.md)
  : Get bullet phases

- [`get_runs()`](https://heike.github.io/bulletxtrctr/reference/get_runs.md)
  : Table of the number of runs

- [`get_sig_lags()`](https://heike.github.io/bulletxtrctr/reference/get_sig_lags.md)
  : Get lags between signatures

- [`hamby252demo`](https://heike.github.io/bulletxtrctr/reference/hamby252demo.md)
  : hamby252demo

- [`hamby252demo_github`](https://heike.github.io/bulletxtrctr/reference/hamby252demo_github.md)
  : hamby252demo_github

- [`land_cc()`](https://heike.github.io/bulletxtrctr/reference/land_cc.md)
  : Get a specified cross section

- [`max_u()`](https://heike.github.io/bulletxtrctr/reference/max_u.md) :
  Wilcox test of bullet to bullet similarity

- [`phase_test()`](https://heike.github.io/bulletxtrctr/reference/phase_test.md)
  : Bullet Phase test

- [`raw_sig_smooth()`](https://heike.github.io/bulletxtrctr/reference/raw_sig_smooth.md)
  : Smooth the raw signature

- [`read_bullet()`](https://heike.github.io/bulletxtrctr/reference/read_bullet.md)
  : Reading all x3p scans belonging to a single bullet from a folder

- [`read_dir()`](https://heike.github.io/bulletxtrctr/reference/read_dir.md)
  : Reading all x3p scans belonging to a folder

- [`rtrees`](https://heike.github.io/bulletxtrctr/reference/rtrees.md) :
  randomforest

- [`runExample()`](https://heike.github.io/bulletxtrctr/reference/runExample.md)
  : Execute the Shiny App for bullet investigation

- [`sig_align()`](https://heike.github.io/bulletxtrctr/reference/sig_align.md)
  : Align two surface cross cuts according to maximal correlation

- [`sig_cms_max()`](https://heike.github.io/bulletxtrctr/reference/sig_cms_max.md)
  : Identify the number of maximum CMS between two signatures

- [`sig_get_peaks()`](https://heike.github.io/bulletxtrctr/reference/sig_get_peaks.md)
  : Identify the location and the depth of peaks and valleys in a
  signature

- [`smoothloess()`](https://heike.github.io/bulletxtrctr/reference/smoothloess.md)
  : Predict smooth from a fit

- [`striation_identify_matches()`](https://heike.github.io/bulletxtrctr/reference/striation_identify_matches.md)
  : Match striation marks across two aligned signatures

- [`switch_xy()`](https://heike.github.io/bulletxtrctr/reference/switch_xy.md)
  : Switch x and y in a data frame

- [`x3p_crosscut()`](https://heike.github.io/bulletxtrctr/reference/x3p_crosscut.md)
  : Read a crosscut from a 3d surface file

- [`x3p_crosscut_optimize()`](https://heike.github.io/bulletxtrctr/reference/x3p_crosscut_optimize.md)
  : Identify a reliable cross section

- [`x3pheader_to_microns()`](https://heike.github.io/bulletxtrctr/reference/x3pheader_to_microns.md)
  : Convert x3p header information to microns from meters
