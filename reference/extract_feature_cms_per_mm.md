# Extract scaled number of consecutively matching striation marks (peaks and valleys) from two aligned signatures

Extract scaled number of consecutively matching striation marks (peaks
and valleys) from two aligned signatures

## Usage

``` r
extract_feature_cms_per_mm(striae, aligned, resolution)
```

## Arguments

- striae:

  data frame of striation marks based on two aligned signatures

- aligned:

  two aligned signatures

- resolution:

  micron per pixel resolution

## Value

scaled number of consecutively matching striation marks (peaks and
valleys)

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
extract_feature_cms(striae$lines)
} # }
```
