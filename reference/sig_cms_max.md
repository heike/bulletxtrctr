# Identify the number of maximum CMS between two signatures

adapted from `bulletGetMaxCMS` aligns two signatures, identifies peaks
and valleys, matches striae, and counts longest run.

## Usage

``` r
sig_cms_max(aligned, span = 35)
```

## Arguments

- aligned:

  data frame of location and aligned signatures

- span:

  positive number for the smoothfactor to use for assessing peaks.

## Value

list of matching parameters, data set of the identified striae, and the
aligned data sets.

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
} # }
```
