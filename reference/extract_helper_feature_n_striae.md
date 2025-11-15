# Extract information for striation marks from two aligned signatures

internal function, called by multiple extract_feature functions

## Usage

``` r
extract_helper_feature_n_striae(striae, type = "peak", match = TRUE)
```

## Arguments

- striae:

  data frame of striation marks based on two aligned signatures

- type:

  one of "peak", "valley" or "all"

- match:

  binary setting: TRUE for matching striae, FALSE for non-matching
  striae

## Value

number of striae

## See also

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
[`extract_features_all()`](https://heike.github.io/bulletxtrctr/reference/extract_features_all.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the data up to be read in, cleaned, etc.
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
extract_helper_feature_n_striae(striae$lines, type = "all", match = T)
} # }
```
