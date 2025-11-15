# Extract results from test in tidy form

`tidy.phase.test` expands the tidy method for test.phase objects. It
gives a summary of the relevant parameters and estimates.

## Usage

``` r
tidy(x, ...)
```

## Arguments

- x:

  phase.test object as returned from `phase_test`

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
