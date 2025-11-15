# Extract features from aligned signatures (legacy)

Extract features from aligned signatures (legacy)

## Usage

``` r
extract_features_all_legacy(res, resolution, tmpfile = NULL)
```

## Arguments

- res:

  list consisting of data frames of lines and aligned signatures, result
  from `sig_cms_max` XXX this needs some fixing

- resolution:

  resolution at which the scans were taken in microns per pixel

- tmpfile:

  character value consisting of a path to a temporary file. If not NULL,
  a line is added to this file each time this function is executed.

## Value

data frame with variables ccf, rough_cor, D, sd_D, matches, mismatches,
cms, non_cms, and sum_peaks
