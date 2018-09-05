bulletxtrctr
================
Heike Hofmann, Susan Vanderplas, Eric Hare, Ganesh Krishnan
September 05, 2018

[![CRAN
Status](http://www.r-pkg.org/badges/version/bulletxtrctr)](https://cran.r-project.org/package=bulletxtrctr)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/bulletxtrctr)](http://www.r-pkg.org/pkg/bulletxtrctr)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build
Status](https://travis-ci.org/heike/bulletxtrctr.svg?branch=master)](https://travis-ci.org/heike/bulletxtrctr)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--09--05-yellowgreen.svg)](/commits/master)
[![Coverage
status](https://codecov.io/gh/heike/bulletxtrctr/branch/master/graph/badge.svg)](https://codecov.io/github/heike/bulletxtrctr?branch=master)

# bulletxtrctr <img src="man/figures/bulletxtrctr.png" align="right" width = "120"/>

Analyze bullet striations using nonparametric methods

## Comparing lands from two bullets

Striae comparisons between bullets are based on land-to-land
comparisons.

1.  Load libraries

<!-- end list -->

``` r
library(dplyr)
library(bulletxtrctr)
library(x3ptools)
library(randomForest)
library(ggplot2)
```

2.  `bulletxtrctr` only works on x3p files. See package `x3ptools` at
    <https://heike.github.io/x3ptools/> for ways to convert different
    file formats into x3p standard files. The NIST Research Ballistics
    Toolmarks data base
    (NRBTD)\[<https://tsapps.nist.gov/NRBTD/Studies/Search>\] provides
    access to scans of bullets and cartridge cases from various case
    studies.

In this tutorial, we’ll work with two bullets from a single barrel of
the Hamby 252 data set. Links to the 12 scans of bullet lands in x3p
format are provided in the `hamby252demo` object.

These commands will read in the bullets directly from the NRBTD
repository, without downloading the files into your working directory:

``` r
b1 <- read_bullet(urllist = hamby252demo[[1]])
b2 <- read_bullet(urllist = hamby252demo[[2]])
```

If instead we wanted to download the files into a folder named “data” in
our working directory, we would run this sequence of commands:

``` r
if (!dir.exists("README_files/data")) {
  dir.create("README_files/data")
}
if (!file.exists("README_files/data/Bullet1/Hamby252_Barrel1_Bullet1_Land1.x3p")) {
  NRBTDsample_download("README_files/data")
}
b1 <- read_bullet("README_files/data/Bullet1")
```

    ## 6 files found. Reading ...

``` r
b2 <- read_bullet("README_files/data/Bullet2")
```

    ## 6 files found. Reading ...

Combine the results into a single data frame:

``` r
b1$bullet <- 1
b2$bullet <- 2
b1$land <- 1:6
b2$land <- 1:6
bullets <- rbind(b1, b2)
```

We expect data to be recorded at the micron level. The scans posted give
measurements in meters:

``` r
bullets$x3p[[1]]$header.info$incrementY
```

    ## [1] 1.5625e-06

``` r
bullets$x3p[[1]]$header.info$incrementX
```

    ## [1] 1.5625e-06

``` r
summary(as.vector(bullets$x3p[[1]]$surface.matrix))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0       0       0       0       0       0   24829

Change measurements to microns:

``` r
bullets <- bullets %>% mutate(
  x3p = x3p %>% purrr::map(.f = function(x) {
    # make sure all measurements are in microns
    x$surface.matrix <- x$surface.matrix*10^6
    x$header.info$incrementY <- x$header.info$incrementY*10^6
    x$header.info$incrementX <- x$header.info$incrementX*10^6
    x
  })
)
```

<!-- This could also be done with the `x3pheader_to_microns` function.  -->

``` r
bullets$x3p[[1]]$header.info$incrementY
```

    ## [1] 1.5625

``` r
bullets$x3p[[1]]$header.info$incrementX
```

    ## [1] 1.5625

``` r
summary(as.vector(bullets$x3p[[1]]$surface.matrix))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   1.513 117.626 166.723 155.933 199.429 216.341   24829

We are working under the assumption that the scans are aligned such that
the base of the bullet are at the bottom (y = 0) of the image, and the
land engraved area is displayed left to right from groove to groove,
i.e. we are assuming that (0,0) is in the bottom left corner of the
image. In scans where no adjustment was made for the barrel’s twist (not
recommended) the twist will be visible in the
image.

``` r
image_x3p(bullets$x3p[[1]], file = "README_files/static/temp-before.png")
```

The raw scan needs to be flipped such that the heel is along the bottom
of the image rather than along the left hand side. ![Raw scan - needs to
be rotated.](README_files/static/temp-before.png)

``` r
# turn the scans such that (0,0) is bottom left
bullets <- bullets %>% mutate(
  x3p = x3p %>% purrr::map(.f = function(x) x %>% 
                             rotate_x3p(angle = -90) %>%
                             y_flip_x3p())
) 
```

``` r
image_x3p(bullets$x3p[[1]], file = "README_files/static/temp-after.png")
```

Scan after the transformation: a clear right twist is visible in the
right slant of striae and grooves.

![Scan after rotation, a clear right twist is visible in the right slant
of the left and right shoulders.](README_files/static/temp-after.png)

3.  Get the ideal cross sections

<!-- end list -->

``` r
bullets <- bullets %>% mutate(
  crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
)
# now extract the crosscuts
bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = crosscut, 
                       .f = x3p_crosscut)
)
```

Visualize the cross cuts:

``` r
crosscuts <- bullets %>% tidyr::unnest(ccdata)
crosscuts %>% 
  ggplot(aes(x = x, y = value)) + 
  geom_line() +
  facet_grid(bullet~land, labeller="label_both") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Note the rather strange cross cut for land 6 in bullet 1. We can look at
the scan - and find quite pronounced tank rash. However, the extraction
of the land is at a height of 375, which is not as much affected by the
rash as the base of the bullet or the top of the scanning area.

    filter(bullets, land==6, bullet==1)$x3p[[1]] %>% image_x3p()

4.  Get the groove locations

<!-- end list -->

``` r
bullets <- bullets %>% mutate(
  grooves = ccdata %>% 
    purrr::map(.f = cc_locate_grooves, method = "middle", 
               adjust = 30, return_plot = TRUE)
)
```

Visualize that the grooves are identified correctly (at least enough to
not distort the final result):

``` r
gridExtra::grid.arrange(
  bullets$grooves[[1]]$plot, bullets$grooves[[2]]$plot,
  bullets$grooves[[3]]$plot, bullets$grooves[[4]]$plot,
  bullets$grooves[[5]]$plot, bullets$grooves[[6]]$plot,
  bullets$grooves[[7]]$plot, bullets$grooves[[8]]$plot,
  bullets$grooves[[9]]$plot, bullets$grooves[[10]]$plot,
  bullets$grooves[[11]]$plot, bullets$grooves[[12]]$plot,
  ncol = 6
)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

5.  Extract signatures

<!-- end list -->

``` r
bullets <- bullets %>% mutate(
  sigs = purrr::map2(
    .x = ccdata, .y = grooves, 
    .f = function(x, y) {
      cc_get_signature(
        ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
    })
)
```

``` r
signatures <- bullets %>% select(source, sigs) %>% tidyr::unnest()
signatures %>% 
  filter(!is.na(sig),!is.na(raw_sig)) %>%
  ggplot(aes(x = x)) + 
  geom_line(aes(y = raw_sig), colour = "grey70") +
  geom_line(aes(y = sig), colour = "grey30") +
  facet_wrap(~source, ncol = 6) +
  ylim(c(-5,5)) +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

8.  Detect peaks and valleys in the aligned signatures

<!-- end list -->

``` r
bullets$bulletland <- paste0(bullets$bullet,"-", bullets$land)
lands <- unique(bullets$bulletland)
comparisons <- data.frame(
  expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)

comparisons <- comparisons %>% mutate(
  aligned = purrr::map2(.x = land1, .y = land2, .f = function(xx, yy) {
    land1 <- bullets$sigs[bullets$bulletland == xx][[1]]
    land2 <- bullets$sigs[bullets$bulletland == yy][[1]]
    land1$bullet <- "first-land"
    land2$bullet <- "second-land"
    
    sig_align(land1$sig, land2$sig)
  })
)
```

Some features are based on aligned signatures:

``` r
comparisons <- comparisons %>% mutate(
  ccf0 = aligned %>% 
    purrr::map_dbl(.f = function(x) extract_feature_ccf(x$lands)),
  lag0 = aligned %>% 
    purrr::map_dbl(.f = function(x) extract_feature_lag(x$lands)),
  D0 = aligned %>% 
    purrr::map_dbl(.f = function(x) extract_feature_D(x$lands)),
  length0 = aligned %>% 
    purrr::map_dbl(.f = function(x) extract_feature_length(x$lands)),
  overlap0 = aligned %>% 
    purrr::map_dbl(.f = function(x) extract_feature_overlap(x$lands))
)
```

Other features need an evaluation of striation marks between two aligned
signatures:

``` r
comparisons <- comparisons %>% mutate(
  striae = aligned %>% purrr::map(.f = sig_cms_max, span = 75) 
)
```

``` r
comparisons <- comparisons %>% mutate(
  matches0 = striae %>% purrr::map_dbl(.f = function(s) {
    bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = TRUE)
  }),
  mismatches0 = striae %>% purrr::map_dbl(.f = function(s) {
    bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = FALSE)
  })
  
)
```

``` r
comparisons <- comparisons %>% mutate(
  bulletA = gsub("([1-2])-([1-6])","\\1",land1),
  bulletB = gsub("([1-2])-([1-6])","\\1",land2),
  landA = gsub("([1-2])-([1-6])","\\2",land1),
  landB = gsub("([1-2])-([1-6])","\\2",land2)
)
```

9.  Extract Features

<!-- end list -->

``` r
comparisons <- comparisons %>% mutate(
  features = purrr::map2(.x = aligned, .y = striae, .f = extract_features_all),
#   res = purrr::pmap(
#     list(aligned, striae, features),
#     function(a, b, c, d) list(lands = a$lands, lines = b$lines, 
#                               maxCMS = c$cms,
#                               ccf = c$ccf, lag = c$lag))
)

comparisons <- comparisons %>% mutate(
  legacy_features = purrr::map(striae, extract_features_all_legacy, resolution = 1.5625)
)

comparisons <- comparisons %>% tidyr::unnest(legacy_features) 
# scale features before using them in the random forest, legacy features can be used out of the box


# quick visualization:
comparisons %>% 
  ggplot(aes(x = landA, y = landB, fill = ccf)) +
  geom_tile() +
  scale_fill_gradient2(low = "grey80", high = "darkorange", 
                       midpoint = 0.5) +
  facet_grid(bulletB~bulletA, labeller = "label_both") +
  xlab("Land A") +
  ylab("Land B")
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

10. Get Score predictions for each land to land
comparison

<!-- end list -->

``` r
comparisons$rfscore <- predict(bulletr::rtrees, newdata = comparisons, type = "prob")[,2]

comparisons %>% 
  ggplot(aes(x = landA, y = landB, fill = rfscore)) +
  geom_tile() +
  scale_fill_gradient2(low = "grey80", high = "darkorange", 
                       midpoint = .5) +
  facet_grid(bulletB~bulletA, labeller = "label_both") +
  xlab("Land A") +
  ylab("Land B")
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

11. Determine bullet-to-bullet scores

<!-- end list -->

``` r
parse_number <- readr::parse_number
bullet_scores <- comparisons %>% group_by(bulletA, bulletB) %>% tidyr::nest()
bullet_scores <- bullet_scores %>% mutate(
  bullet_score = data %>% purrr::map_dbl(
    .f = function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore)))
)
bullet_scores %>% select(-data)
```

    ## # A tibble: 4 x 3
    ##   bulletA bulletB bullet_score
    ##   <chr>   <chr>          <dbl>
    ## 1 1       1              0.982
    ## 2 2       1              0.681
    ## 3 1       2              0.681
    ## 4 2       2              0.989

An interactive interface for doing comparisons is available
<https://oaiti.org/apps/bulletmatcher/>
