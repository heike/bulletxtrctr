# Fit a loess curve to a bullet data frame

A loess regression is fit to the surface measurements and residuals are
calculated. The most extreme 0.25% of residuals are filtered from
further consideration. The result is called the raw signature of the
bullet land. Adapted from `fit_loess` in `bulletr`

## Usage

``` r
cc_fit_loess(ccdata, span = 0.75)
```

## Arguments

- ccdata:

  The crosscut as returned from x3p_to_df, grooves need to be removed
  ahead of time

- span:

  The span to use for the loess regression

## Value

a list of a data frame of the original bullet measurements extended by
loess fit, residuals, and standard errors and two plots: a plot of the
fit, and a plot of the bullet's land signature.

## Examples

``` r
library(dplyr)
ccdata <- data_frame(
  x = seq(0, 6, .001),
  value = 10 - (3 - x)^2 + rnorm(length(x), sd = .25)
)
cc_fit_loess(ccdata = ccdata)
#> # A tibble: 6,001 × 7
#>        x value fitted  raw_sig     se abs_resid chop 
#>    <dbl> <dbl>  <dbl>    <dbl>  <dbl>     <dbl> <lgl>
#>  1 0     0.549   1.01 -0.464   0.0123   0.464   FALSE
#>  2 0.001 1.29    1.02  0.268   0.0123   0.268   FALSE
#>  3 0.002 0.746   1.02 -0.279   0.0123   0.279   FALSE
#>  4 0.003 0.806   1.03 -0.224   0.0123   0.224   FALSE
#>  5 0.004 0.824   1.04 -0.213   0.0122   0.213   FALSE
#>  6 0.005 1.04    1.04 -0.00465 0.0122   0.00465 FALSE
#>  7 0.006 0.715   1.05 -0.333   0.0122   0.333   FALSE
#>  8 0.007 0.460   1.05 -0.594   0.0122   0.594   FALSE
#>  9 0.008 1.07    1.06  0.0106  0.0122   0.0106  FALSE
#> 10 0.009 1.28    1.07  0.210   0.0122   0.210   FALSE
#> # ℹ 5,991 more rows
```
