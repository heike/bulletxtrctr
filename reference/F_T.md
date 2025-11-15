# Reference distribution abd debsity for the test statistic between Same-source and Different-source averages using phase selection

Assuming an overall sample size of 36 (for 6 x 6 comparisons, and n=6
independent objects), we are interested in the distribution of T defined
as the difference between the average of the same source values and the
median average (XXX see paper for definition) of the different source
values: \\T = \bar{X}\_{SS} - \tilde{X}\_{DS}\\, where groups SS and DS
are set by phase selection (after inspecting the set of n values). For a
(one-sided) hypothesis test of \\H_0\\: no difference between the means
and \\H_1:\\: the same-source values are larger on average than the
different source values, the test statistic T has under the null
hypothesis and the group-finding strategy \\\cal G\\ a distribution
given as \$\$ P(T \< t \mid H_0, {\cal G}) = C \cdot
\int\_{-\infty}^{\infty} F^2(x) f(x) \left\[ F(x + t) - F(x) \right\]^3
dx\$\$ where F is the (approximately normal) distribution of
\\\bar{X}\_{SS}\\ under \\H_0\\ with expected value of \\\mu\\
(\\=E\[\bar{X}\_{SS}\] = E\[\bar{X}\_{DS}\]\\) and standard deviation
\\\sigma\\ (standard deviation of a group of size \$n\$ under \$H_0\$).

## Usage

``` r
F_T(t, sigma = 1, n = 6, lower.tail = TRUE)

f_T(t, sigma = 1, n = 6)
```

## Arguments

- t:

  value of the observed test statistic, t has to be non-negative.

- sigma:

  (estimated) standard deviation of an average of size n of the
  (original) sample

- n:

  integer value, number of values in the same-source group.

- lower.tail:

  should the lower tail probability be returned?

## Examples

``` r
# example code
F_T(0.2, sigma = 0.1, lower.tail = FALSE) # only multitude of sigma informs the test
#> [1] 0.2066704
#> attr(,"abs.error")
#> [1] 1.602902e-07
#> attr(,"message")
#> [1] "OK"
F_T(2, lower.tail = FALSE)
#> [1] 0.2066704
#> attr(,"abs.error")
#> [1] 1.602902e-07
#> attr(,"message")
#> [1] "OK"
F_T(3, lower.tail = FALSE)
#> [1] 0.02398655
#> attr(,"abs.error")
#> [1] 1.413966e-07
#> attr(,"message")
#> [1] "OK"
F_T(4, lower.tail = FALSE)
#> [1] 0.001271115
#> attr(,"abs.error")
#> [1] 9.966644e-09
#> attr(,"message")
#> [1] "OK"
# critical values of t distribution and F_T (for sigma = 1):
F_T(qt(1-0.05, df=34), lower.tail = FALSE)
#> [1] 0.3381702
#> attr(,"abs.error")
#> [1] 1.176617e-07
#> attr(,"message")
#> [1] "OK"
F_T(qt(1-0.05/6, df=34), lower.tail = FALSE)
#> [1] 0.07501263
#> attr(,"abs.error")
#> [1] 2.498902e-07
#> attr(,"message")
#> [1] "OK"
F_T(qt(1-0.01/6, df=34), lower.tail = FALSE)
#> [1] 0.01590425
#> attr(,"abs.error")
#> [1] 8.744108e-08
#> attr(,"message")
#> [1] "OK"

x <- 10^-(1:4)
x <- sort(c(x, x/2))
t_q <- qt(1-x, df=34)
adjusted <- F_T(t_q, lower.tail=FALSE)
```
