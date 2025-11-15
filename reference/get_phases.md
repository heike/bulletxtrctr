# Get bullet phases

Note that the combination of `land1` and `land2` are a key to the
land-to-land scores, i.e. if a bullet has six lands, each of the input
vectors should have 36 entries. In the case that the lands are not
factor variables, the order in which different lands are encountered in
the vector is used to determine their order.

## Usage

``` r
get_phases(land1, land2)
```

## Arguments

- land1:

  vector with land ids of bullet 1

- land2:

  vector with land ids of bullet 2

## Value

numeric vector of the phases (diagonal groupings). The number of the
phase is determined by the comparison with land 1 on the first bullet,
i.e. phase 1 is the diagonal, that contains the land-to-land comparison
with bullet 2 land 1, phase 2 contains B1-L1 vs B2-L2, Phase 3 contains
B1-L1 vs B2-L3, etc.
