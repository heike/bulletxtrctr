# Table of the number of runs

Identify the length of runs (of values TRUE) and their frequencies.

## Usage

``` r
get_runs(x)
```

## Arguments

- x:

  Boolean vector

## Value

a table of the number of runs of TRUEs

## Examples

``` r
x <- rbinom(100, size = 1, prob = 1 / 3)
get_runs(x == 1) # expected value for longest run is 3
#> 
#>  1  2  4  5 
#> 15  5  1  1 
get_runs(x == 0) # expected value for longest run is 6
#> 
#> 1 2 3 4 5 6 
#> 4 7 5 3 3 1 
```
