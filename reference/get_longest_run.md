# Length of the longest run of TRUEs

Identifies the length of the longest run of TRUEs in Boolean vector `x`.
used to be `maxCMS`

## Usage

``` r
get_longest_run(x)
```

## Arguments

- x:

  Boolean vector

## Value

an integer value of the length of the longest run of TRUE values

## Examples

``` r
x <- rbinom(100, size = 1, prob = 1 / 3)
get_runs(x == 1) # expected value for longest match is 3
#> 
#>  1  2  3  4 
#> 13  6  1  1 
get_longest_run(x == 1)
#> [1] 4
```
