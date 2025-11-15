# Print information of a phase test

`print.phase.test` expands the generic print method for x3p objects. It
gives a summary of the most relevant x3p meta information and returns
the object invisibly.

## Usage

``` r
# S3 method for class 'phase.test'
print(x, ...)
```

## Arguments

- x:

  phase.test object

- ...:

  ignored

## Examples

``` r
logo <- x3ptools::x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
print(logo)
#> x3p object
#> Instrument: N/A 
#> Model: N/A 
#> Date: 2018-01-30T08:30:24 
#> size (width x height): 741 x 419 in pixel 
#> resolution: 6.4500e-07 x 6.4500e-07 
#> Creator: Heike Hofmann, CSAFE 
#> Comment: image rendered from the CSAFE logo 
```
