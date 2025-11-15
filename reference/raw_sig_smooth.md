# Smooth the raw signature

Smooth the raw signature

## Usage

``` r
raw_sig_smooth(sig, span = 0.03, limits = c(-5, 5))
```

## Arguments

- sig:

  numeric vector of the raw signature

- span:

  width of the smoother, defaults to 0.03. XXX Should be

- limits:

  vector of the form c(min, max). Results will be limited to be between
  these values.

## Value

numeric vector of the same length as the input with the smoothed
signature.

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
