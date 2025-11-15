# Read a crosscut from a 3d surface file

Read a crosscut from a 3d surface file

## Usage

``` r
x3p_crosscut(x3p, y = NULL, range = 1e-05)
```

## Arguments

- x3p:

  if character, path to an x3p file. Otherwise a scan in x3p format is
  expected.

- y:

  level of the crosscut to be taken. If this level does not exist, the
  crosscut along the middle of the land is returned.

- range:

  numeric specifying a range of (y, y + range) to be extracted

## Value

data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the data up to be read in, cleaned, etc.
library(bulletxtrctr)
library(x3ptools)
library(ggplot2)

example_data <- bullet_pipeline(
  location = list(Bullet1 = c(hamby252demo$bullet1[3])),
  stop_at_step = "clean",
  x3p_clean = function(x) x %>%
      x3p_scale_unit(scale_by=10^6) %>%
      rotate_x3p(angle = -90) %>%
      y_flip_x3p()
)

x3p_crosscut_optimize(example_data$x3p[[1]])
x3p_crosscut(example_data$x3p[[1]], 75) %>%
  ggplot(aes(x = x, y = value)) + geom_line()
} # }
```
