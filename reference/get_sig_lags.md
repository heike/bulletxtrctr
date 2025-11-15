# Get lags between signatures

Extract lags for aligning all signatures in a set with respect to the
signature specified in 'align_to'. Alignments will vary depending on
which signature is used for alignment.

## Usage

``` r
get_sig_lags(
  data,
  value,
  sig_id,
  min.overlap = 500,
  align_to = levels(sig_id)[1],
  resolution = 0.645,
  tol = 1e-06
)
```

## Arguments

- data:

  data frame with ids of signatures, x variable and `value` variable.

- value:

  symbol for the signature values

- sig_id:

  factor variable with signatures ids

- min.overlap:

  value passed on to
  [`?get_ccf`](https://heike.github.io/bulletxtrctr/reference/get_ccf.md).

- align_to:

  id of the signature used for alignment. Defaults to the first factor
  level.

- resolution:

  numeric value representing the resolution of the signature in microns
  per pixel.

- tol:

  tolerance used, i.e. smallest value, such that 1 + tol not equal to 1.
  Can't be smaller than the machine's numeric resolution
  `.Machine$double.eps`.

## Value

data frame with a summary (one row for each level of `sig_id`)
containing the information on `id`, `lag`, and `ccf`.

## Examples

``` r
if (FALSE) { # \dontrun{
tmaRks::data("toolmarks", package="tmaRks")
tnest <- toolmarks %>% filter(size=="L") %>% group_by(side, angle) %>% tidyr::nest()
tnest <- tnest %>% mutate(
  lags = data %>% purrr::map(.f = get_sig_lags, value=signature,
              sig_id=TID, min.overlap=400, resolution = 7.0505e-03)
)
} # }
```
