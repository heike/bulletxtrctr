# Align two surface cross cuts according to maximal correlation

The bullet with the first name serves as a reference, the second bullet
is shifted.

## Usage

``` r
sig_align(sig1, sig2, min.overlap = NULL)
```

## Arguments

- sig1:

  vector of first signature

- sig2:

  vector of second signature

- min.overlap:

  additional parameter passed on to get_ccf

## Value

list consisting of a) the maximal cross correlation, b) the lag
resulting in the highest cross correlation, and c) same data frame as
input, but y vectors are aligned for maximal correlation d) a vector of
cross-correlation values

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

sig_align(example_data$sigs[[1]]$sig, example_data$sigs[[2]]$sig)
} # }
```
