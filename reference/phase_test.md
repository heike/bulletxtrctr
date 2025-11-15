# Bullet Phase test

Determine a p-value for the strength of similarity between two bullets
based on scores for each land-to-land comparison.

## Usage

``` r
phase_test(land1, land2, score, sigma_0 = NA)
```

## Arguments

- land1:

  vector with land ids of the LEAs in bullet 1

- land2:

  vector with land ids of the LEAs in bullet 2

- score:

  real-valued vector consisting of land-to-land comparisons. Higher
  values are assumed to describe higher similarity.

- sigma_0:

  real valued, standard deviation of the different source distribution.
  If `NA` (default), the standard deviation of the scores corresponding
  to non-matches (after selecting the highest phase) is taken.
  Real-values are interpreted as fixed standard deviation. The user can
  provide their own function of the form `function (data)`, where `data`
  is a data frame with `land1, land2, score,` and `phase`. The `phase`
  vector consists of integer values 1, ..., `k`, where `k` is the
  maximum of the number of unique lands in land1 and land2. The values
  are ordered such that the highest value `k` of phase corresponds to
  the elements in score with the highest average score.

## Value

phase.test object. List of estimate=est1-est2,estimate1=est1,
estimate2=est2, statistic= test.statistic, p.value=pvalue,
parameter=sigma_0, data = dframe
