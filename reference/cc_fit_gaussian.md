# Use a gaussian filter on bullet data frame

A gaussian filter is fit to the surface measurements and residuals are
calculated. The most extreme 0.25\\ The result is called the raw
signature of the bullet land.

## Usage

``` r
cc_fit_gaussian(ccdata, span = 600)
```

## Arguments

- ccdata:

  The crosscut as returned from x3p_to_df

- span:

  The size, in microns, of the smoothing window. Defaults to 600, which
  is 24\\ 9mm land.

## Value

a list of a data frame of the original bullet measurements extended by
gaussian filtration, residuals, and two plots: a plot of the fit, and a
plot of the bullet's land signature.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
ccdata <- data_frame(
  x = seq(0, 6000, 1),
  value = 10 - (3 - x / 1000)^2 + rnorm(length(x), sd = .25)
)
#> Warning: `data_frame()` was deprecated in tibble 1.1.0.
#> ℹ Please use `tibble()` instead.
cc_fit_gaussian(ccdata = ccdata)
#> # A tibble: 6,001 × 7
#>        x value fitted raw_sig se    abs_resid chop 
#>    <dbl> <dbl>  <dbl>   <dbl> <lgl>     <dbl> <lgl>
#>  1     0 0.391   1.55  -1.16  NA        1.16  TRUE 
#>  2     1 1.00    1.56  -0.551 NA        0.551 FALSE
#>  3     2 1.17    1.56  -0.390 NA        0.390 FALSE
#>  4     3 1.31    1.56  -0.255 NA        0.255 FALSE
#>  5     4 0.569   1.56  -0.994 NA        0.994 TRUE 
#>  6     5 0.968   1.56  -0.596 NA        0.596 FALSE
#>  7     6 0.975   1.57  -0.592 NA        0.592 FALSE
#>  8     7 0.971   1.57  -0.598 NA        0.598 FALSE
#>  9     8 0.910   1.57  -0.662 NA        0.662 FALSE
#> 10     9 1.21    1.57  -0.363 NA        0.363 FALSE
#> # ℹ 5,991 more rows
```
