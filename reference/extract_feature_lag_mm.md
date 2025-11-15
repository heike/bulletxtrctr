# Extract lag in mm from two (or more) aligned signatures

Extract lag in mm from two (or more) aligned signatures

## Usage

``` r
extract_feature_lag_mm(aligned, resolution)
```

## Arguments

- aligned:

  data frame with variable x (for location) and two or more measurements
  (the bullets entry in the list returned from sig_align)

- resolution:

  numeric value of resolution in micron per pixel

## Value

(vector) of lags in millimeter

## See also

Other alignment-related-features:
[`extract_feature_D()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_D.md),
[`extract_feature_ccf()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_ccf.md),
[`extract_feature_lag()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_lag.md),
[`extract_feature_length()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length.md),
[`extract_feature_length_mm()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_length_mm.md),
[`extract_feature_overlap()`](https://heike.github.io/bulletxtrctr/reference/extract_feature_overlap.md),
[`extract_features_all()`](https://heike.github.io/bulletxtrctr/reference/extract_features_all.md)
