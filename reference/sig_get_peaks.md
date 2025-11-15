# Identify the location and the depth of peaks and valleys in a signature

Adapted from the function `get_peaks`

## Usage

``` r
sig_get_peaks(
  sig,
  smoothfactor = 35,
  striae = TRUE,
  window = TRUE,
  plot = TRUE
)
```

## Arguments

- sig:

  numeric vector of smoothed signature, padded with NAs at front and/or
  back.

- smoothfactor:

  set to default of 35. Smaller values will pick up on smaller changes
  in the crosscut.

- striae:

  If TRUE, show the detected striae on the plot

- window:

  If TRUE, show the window of the striae on the plot

- plot:

  If TRUE, show a plot

## Value

list of several objects:

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the data up to be read in, cleaned, etc.
library(bulletxtrctr)
library(x3ptools)
library(ggplot2)

example_data <- bullet_pipeline(
  location = list(Bullet1 = c(hamby252demo$bullet1[3])),
  x3p_clean = function(x) x %>%
      x3p_scale_unit(scale_by=10^6) %>%
      rotate_x3p(angle = -90) %>%
      y_flip_x3p()
)

sig <- cc_get_signature(example_data$ccdata[[1]], example_data$grooves[[1]])
sig_peaks <- sig_get_peaks(sig$sig)
sig_peaks[1:6]
# Plot
sig_peaks$plot
} # }
```
