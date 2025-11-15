# Extract features from aligned signatures

Extract features from aligned signatures

## Usage

``` r
extract_features_all(aligned, striae, resolution, tmpfile = NULL, ...)
```

## Arguments

- aligned:

  aligned signatures, result from `sig_cms_max`

- striae:

  data frame with evaluated matching striae

- resolution:

  micron per pixel resolution of scans

- tmpfile:

  character consisting of the path to a temporary file. If not `NULL`,
  one line is appended to the file each time the function is executed.

- ...:

  passed on to extractor functions

## See also

Other alignment-related-features:
[`extract_feature_D()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_D.md),
[`extract_feature_ccf()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_ccf.md),
[`extract_feature_lag()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_lag.md),
[`extract_feature_lag_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_lag_mm.md),
[`extract_feature_length()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length.md),
[`extract_feature_length_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length_mm.md),
[`extract_feature_overlap()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_overlap.md)

Other striae-related-features:
[`extract_feature_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms.md),
[`extract_feature_cms2()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms2.md),
[`extract_feature_cms2_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_cms2_per_mm.md),
[`extract_feature_left_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_left_cms.md),
[`extract_feature_matches()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_matches.md),
[`extract_feature_matches_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_matches_per_mm.md),
[`extract_feature_mismatches()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_mismatches.md),
[`extract_feature_mismatches_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_mismatches_per_mm.md),
[`extract_feature_non_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_non_cms.md),
[`extract_feature_non_cms_per_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_non_cms_per_mm.md),
[`extract_feature_right_cms()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_right_cms.md),
[`extract_feature_sum_peaks()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_sum_peaks.md),
[`extract_helper_feature_n_striae()`](https://heike.github.io/bulletxtrctr/reference/extract_helper_feature_n_striae.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(bulletxtrctr)
library(x3ptools)

example_data <- bullet_pipeline(
  location = list(
    Bullet1 = c(hamby252demo$bullet1[2]),
    Bullet2 = c(hamby252demo$bullet2[4])
  ),
  x3p_clean = function(x) x %>%
      x3p_scale_unit(scale_by=10^6) %>%
      rotate_x3p(angle = -90) %>%
      y_flip_x3p()
)

alignment <- sig_align(example_data$sigs[[1]]$sig,
                       example_data$sigs[[2]]$sig)
striae <- sig_cms_max(alignment)

extract_features_all(alignment, striae)
} # }
```
