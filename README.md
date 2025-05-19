
This is a package for clustering OD desire lines (or flows).

1.  Create distance matrix between OD pairs
2.  Pass distance matrix to a clustering algorithm

# flowcluster

<!-- badges: start -->

[![R-CMD-check](https://github.com/Hussein-Mahfouz/flowcluster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Hussein-Mahfouz/flowcluster/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of flowcluster is to provide functions and example datasets for
clustering OD origin-destination datasets. This process can be useful
for visualising large OD datasets and analysing patterns in the data.

``` bash
# see if the workflows are working:
gh run list
```

Install the package with:

``` r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pkg_install("Hussein-Mahfouz/flowcluster")
```

Or, for local development, clone the repo, open the project in RStudio,
and run:

``` r
devtools::load_all()
#> ℹ Loading flowcluster
```

# Example

``` r
library(flowcluster)
flows_sf <- flows_leeds
head(flows_sf)
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -1.705747 ymin: 53.88337 xmax: -1.346486 ymax: 53.93065
#> Geodetic CRS:  WGS 84
#>      origin destination count                       geometry
#> 1 E02002330   E02002330    30 LINESTRING (-1.398482 53.93...
#> 2 E02002330   E02002331   366 LINESTRING (-1.398482 53.93...
#> 3 E02002330   E02002332     6 LINESTRING (-1.398482 53.93...
#> 4 E02002330   E02002333     2 LINESTRING (-1.398482 53.93...
#> 5 E02002330   E02002334    31 LINESTRING (-1.398482 53.93...
#> 6 E02002330   E02002335    17 LINESTRING (-1.398482 53.93...
plot(flows_sf["count"])
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
flows_clustered <- flowcluster::flowcluster(
  flows_sf
)
#> hello world
```

# Style

We use the tidyverse style. To ensure your changes abide by this, run
the following command before pushing changes:

``` r
styler::style_pkg()
```
