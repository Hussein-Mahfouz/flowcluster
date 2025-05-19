
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
