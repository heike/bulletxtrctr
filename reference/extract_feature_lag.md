# Extract lag from two (or more) aligned signatures

In the case of two signatures the result is an integer definining the
number of indices by which the second signature is shifted compared to
the first signature in the alignment. Note that this lag can be
negative. In the case of multiple signatures the result is a vector of
non-negative integers of the same length as signatures. Each element
gives the number of indices by which the corresponding signature is
shifted compared to the *first* signature. By definition, one of the
numbers has to be 0 indicating the *first* signature. XXX Need to
indicate that "first" isn't the col index

## Usage

``` r
extract_feature_lag(aligned)
```

## Arguments

- aligned:

  data frame with variable x (for location) and two or more measurements
  (the bullets entry in the list returned from sig_align)

## Value

(vector) of lags

## See also

Other alignment-related-features:
[`extract_feature_D()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_D.md),
[`extract_feature_ccf()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_ccf.md),
[`extract_feature_lag_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_lag_mm.md),
[`extract_feature_length()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length.md),
[`extract_feature_length_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length_mm.md),
[`extract_feature_overlap()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_overlap.md),
[`extract_features_all()`](https://heike.github.io/bulletxtrctr/reference/extract_features_all.md)

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

extract_feature_lag(alignment$lands)
} # }
```
