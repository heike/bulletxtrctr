# Extract signature from crosscut

x3p file of a 3d topological bullet surface is processed at surface
crosscut y, measurements outside the bullet grooves in the crosscuts are
left out, and a loess smooth is used (see
[`?loess`](https://rdrr.io/r/stats/loess.html) for details) to remove
the big structure.

## Usage

``` r
cc_get_signature(ccdata, grooves = NULL, span1 = 0.75, span2 = 0.03)
```

## Arguments

- ccdata:

  crosscut as returned from x3p_crosscut

- grooves:

  The grooves to use as a two element vector, if desired

- span1:

  The span for the loess fit to get from the profile to the raw
  signature

- span2:

  The span for the loess fit to smooth the raw signature; if `span2` is
  set to NA, no second smoothing will be done.

## Value

data frame

## Examples

``` r
data(br411)
cc <- x3p_crosscut_optimize(br411)
ccdata <- x3p_crosscut(br411, cc)
grooves <- cc_locate_grooves(ccdata)
signature <- cc_get_signature(ccdata, grooves)

if (FALSE) { # \dontrun{
# Set the data up to be read in, cleaned, etc.
library(bulletxtrctr)
library(x3ptools)

example_data <- bullet_pipeline(
  location = list(Bullet1 = c(hamby252demo$bullet1[3])),
  x3p_clean = function(x) x %>%
      x3p_scale_unit(scale_by=10^6) %>%
      rotate_x3p(angle = -90) %>%
      y_flip_x3p()
)

cc_get_signature(example_data$ccdata[[1]],
                 example_data$grooves[[1]]) %>%
head()
} # }
```
