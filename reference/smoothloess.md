# Predict smooth from a fit

Predict smooth from a fit

## Usage

``` r
smoothloess(y, span)
```

## Arguments

- y:

  numeric vector

- span:

  The span of the loess fit

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
x <- seq(0, 6 * pi, by = .02)
y <- 5.5 * sin(x) + rnorm(length(x))


ggplot() +
  geom_point(aes(x = 1:length(y), y = y), shape = 1) +
  geom_line(aes(x = 1:length(y), y = smoothloess(y, .05)),
    color = "red"
  ) +
  geom_line(aes(x = 1:length(y), y = raw_sig_smooth(y, .05)),
    color = "blue"
  )
} # }
```
