# Match striation marks across two aligned signatures

`striae1` and `striae2` are data frames of previously identified peaks
and valleys returned by `sig_get_peaks`

## Usage

``` r
striation_identify_matches(striae1, striae2)
```

## Arguments

- striae1:

  data frame as returned from sig_get_peaks function. data frames are
  expected to have the following variables: xmin, xmax, group, type,
  bullet, heights

- striae2:

  data frame as returned from sig_get_peaks function. data frames are
  expected to have the following variables: xmin, xmax, group, type,
  bullet, heights#' Check a striae object is valid

## Value

data frame of the same form as lines1 and lines2, but with an additional
variable of whether the striation marks are matches

## Details

adapted from function `striation_identify` in the bulletr package, just
used internally. Not intended for public consumption.
