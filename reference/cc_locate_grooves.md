# Find the grooves of a bullet land

Find the grooves of a bullet land

## Usage

``` r
cc_locate_grooves(
  ccdata,
  method = "rollapply",
  smoothfactor = 15,
  adjust = 10,
  groove_cutoff = 400,
  mean_left = NULL,
  mean_right = NULL,
  mean_window = 100,
  return_plot = F,
  ...
)
```

## Arguments

- ccdata:

  data frame of the crosscut. Data frame needs location x and measured
  values as `value`. If multiple crosscuts are to be considered, include
  a variable y and use as a key.

- method:

  method to use for identifying grooves. One of "quadratic",
  "rollapply", "middle", "logisticlegacy", "lassobasic", "lassofull",
  "bcp". Defaults to "rollapply"

- smoothfactor:

  The smoothing window to use - XXX the smoothing window seems to depend
  on the resolution at which the data has been collected.

- adjust:

  positive number to adjust the grooves - XXX should be expressed in
  microns rather than an index (not used for method = "middle")

- groove_cutoff:

  The index at which a groove cannot exist past - XXX this parameter
  should be expressed in microns rather than as an index to be able to
  properly deal with different resolutions

- mean_left:

  If provided, the location of the average left groove

- mean_right:

  If provided, the location of the average right groove

- mean_window:

  The window around the means to use

- return_plot:

  Return plot of grooves?

- ...:

  parameters passed on to specific groove location methods

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the data up to be read in, cleaned, etc.
library(bulletxtrctr)
library(x3ptools)

example_data <- bullet_pipeline(
  location = list(Bullet1 = c(hamby252demo$bullet1[3])),
  stop_at_step = "crosscut",
  x3p_clean = function(x) x %>%
      x3p_scale_unit(scale_by=10^6) %>%
      rotate_x3p(angle = -90) %>%
      y_flip_x3p()
)

cc_locate_grooves(example_data$ccdata[[1]],
  method = "rollapply",
  adjust = 30, return_plot = T
)
cc_locate_grooves(example_data$ccdata[[1]],
  method = "middle",
  adjust = 30, return_plot = T
)
cc_locate_grooves(example_data$ccdata[[1]],
  method = "quadratic",
  adjust = 30, return_plot = T
)
} # }
```
