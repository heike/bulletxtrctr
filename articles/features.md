# Feature Extraction

The package defines a set of functions of the form `extract_feature_XXX`
where `XXX` stands for a particular feature to be extracted either
directly from a set of aligned signatures or indirectly from peaks and
valleys derived from a set of aligned signatures.

Using the function `extract_features_all` will automatically call all
currently implemented functions of the form `extract_feature_XXX` and
return a dataframe (of a single row) for each set of aligned signatures.

There are two types of features implemented at the moment: features,
that are based on - only the aligned signatures: D, ccf, rough cor, … -
striae extracted from aligned signatures: cms, matches, …

## Distance $D$

The Euclidean distance between two aligned signatures $s$ and $t$ is
defined as
$$d(s,t) = \sqrt{\sum\limits_{i = 1}^{N}\left( s_{i} - t_{i} \right)^{2}}$$,
where $i = 1,...,N$, the length of the aligned signatures. Two
signatures $s$ and $t$ of respective lengths $n_{s}$ and $n_{t}$ can be
aligned by padding one or both of the signatures with missing values
`NA`. The aligned form of signatures $s$ and $t$ then has length
$N \geq n_{s},n_{t}$. $d$ is then a measure of the distance between the
two vectors. Note that this form of $d$ is not invariant to the
resolution $r$, at which signatures $s$ and $t$ are collected. To make
the distance invariant to the resolution, we could use $d \cdot r$ as an
estimate for the area between the two signatures. However, in cases of
degraded signatures (i.e. cases, in which for some reason a signature
cannot be extracted from a whole land), we want to also make distance
invariant to the length of the signatures involved. We therefore define
$$D(s,t) = d(s,t)/N$$ to be the average distance between aligned
signatures $s$ and $t$.

## Consecutively matching striae (CMS)

Consecutively matching striae is a measure first established by Alfred
Biasotti in 1950 (reference). The number of consecutively matching
striae is the number of consecutive *peaks* two signature have in
common, i.e. the valleys in between the peaks should not be counted. XXX
Currently, the function `extract_feature_cms` counts both peaks and
valleys XXX Generally, a CMS of 6 or higher is considered to be strongly
indicative of a match (need another citation for this).

## Countable features

All features that return a count in one way or the other, such as `cms`,
`noncms`, `matches`, `nonmatches`, … are accompanied by functions that
scale these integers by the signature length (to make these numbers
independent from length) and return values scaled to millimeter. The
corresponding variables then have an appendix of `_per_mm`.
