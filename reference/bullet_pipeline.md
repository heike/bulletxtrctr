# Helper file to setup data

Helper file to setup data

## Usage

``` r
bullet_pipeline(location, stop_at_step = NULL, x3p_clean = function(x) x, ...)
```

## Arguments

- location:

  directory or list of urls containing x3p files. If there are
  sub-directories or sub-lists, this function will assume that each
  sub-directory contains lands from different bullets and will separate
  them accordingly

- stop_at_step:

  One of read, clean, crosscut, grooves, signatures

- x3p_clean:

  function to use to clean the x3p file - convert header info to correct
  units, rotate the surface matrix, etc.

- ...:

  additional arguments to cc_locate_grooves and cc_get_signature

## Value

a data_frame with bullet x3p files and processed data steps
