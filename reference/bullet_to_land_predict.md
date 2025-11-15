# Get land to land prediction based on bullet to bullet comparisons

The combination of `land1` and `land2` are a key to the scores, i.e. if
a bullet has six lands, each of the input vectors should have length 36.

## Usage

``` r
bullet_to_land_predict(
  land1,
  land2,
  scores,
  difference,
  alpha = 0.05,
  addNA = FALSE
)
```

## Arguments

- land1:

  (numeric) vector with land ids of bullet 1

- land2:

  (numeric) vector with land ids of bullet 2

- scores:

  numeric vector of scores to be summarized into a single number

- difference:

  numeric value describing the minimal difference between scores from
  same source versus different sources.

- alpha:

  numeric value describing the significance level for the bootstrap

- addNA:

  how are missing values treated? addNA = TRUE leaves missing values,
  addNA=FALSE imputes with 0.

## Value

numeric vector of binary prediction whether two lands are same-source.
Vector has the same length as the input vectors.
