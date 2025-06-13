

# flowcluster

<!-- badges: start -->

[![R-CMD-check](https://github.com/Hussein-Mahfouz/flowcluster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Hussein-Mahfouz/flowcluster/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of flowcluster is to provide minimal functionality for
clustering OD desire lines (or flows). This includes: 1. Creating
distance matrices between OD pairs 2. Passing distance matrices to a
clustering algorithm

## Install from GitHub

You can install the development version of flowcluster from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Hussein-Mahfouz/flowcluster")
```

Load it as follows:

``` r
library(flowcluster)
```

## Usage

The package provides a function to create a distance matrix from a data
frame of OD pairs, and then pass that matrix to a clustering algorithm.
The current distance matrix is an implementation of the flow distance
and flow dissimilarity measures described in (Tao and Thill 2016).

### Data preparation

First, load your package and the sample data, and project it to a metric
CRS.  
This is important for accurate length calculations and spatial
operations.

``` r
library(flowcluster)
library(tidyverse)
library(sf)
library(tmap)

# Load sample flow data and project to metric CRS (e.g., EPSG:3857)
flows_sf <- flows_leeds
flows_sf <- st_transform(flows_sf, 3857)
```

Next, add a column containing the length (in meters) of each flow
line.  
This step also checks that your data is in a projected CRS.

``` r
# Add length (meters) to each flow line
flows_sf <- add_flow_length(flows_sf)
head(flows_sf, 5)
```

    Simple feature collection with 5 features and 4 fields
    Geometry type: LINESTRING
    Dimension:     XY
    Bounding box:  xmin: -189882.9 ymin: 7148099 xmax: -149890.2 ymax: 7157033
    Projected CRS: WGS 84 / Pseudo-Mercator
    # A tibble: 5 × 5
      origin    destination count                               geometry length_m
      <chr>     <chr>       <dbl>                       <LINESTRING [m]>    <dbl>
    1 E02002330 E02002330      30 (-155678.3 7157033, -155678.3 7157033)       0 
    2 E02002330 E02002331     366 (-155678.3 7157033, -149890.2 7155611)    5960.
    3 E02002330 E02002332       6 (-155678.3 7157033, -189882.9 7153431)   34394.
    4 E02002330 E02002333       2 (-155678.3 7157033, -187947.5 7151336)   32768.
    5 E02002330 E02002334      31 (-155678.3 7157033, -151100.3 7148099)   10039.

Filter out flows based on a minimum and maximum length.

``` r
# Filter flows based on length (e.g., between 100 and 10000 meters)
flows_sf = filter_by_length(flows_sf, length_min = 1000, length_max = 20000)
```

Then, extract start and end coordinates for each flow, and assign unique
IDs.  
These are needed for subsequent distance calculations and clustering.

``` r
# Add start/end coordinates and unique flow IDs
flows_sf <- add_xyuv(flows_sf)
head(flows_sf, 5)
```

    Simple feature collection with 5 features and 9 fields
    Geometry type: LINESTRING
    Dimension:     XY
    Bounding box:  xmin: -166579.3 ymin: 7140540 xmax: -149890.2 ymax: 7157033
    Projected CRS: WGS 84 / Pseudo-Mercator
    # A tibble: 5 × 10
      origin    destination count                   geometry length_m       x      y
      <chr>     <chr>       <dbl>           <LINESTRING [m]>    <dbl>   <dbl>  <dbl>
    1 E02002330 E02002331     366 (-155678.3 7157033, -1498…    5960. -1.56e5 7.16e6
    2 E02002330 E02002334      31 (-155678.3 7157033, -1511…   10039. -1.56e5 7.16e6
    3 E02002330 E02002335      17 (-155678.3 7157033, -1636…   10832. -1.56e5 7.16e6
    4 E02002330 E02002349       1 (-155678.3 7157033, -1665…   19770. -1.56e5 7.16e6
    5 E02002330 E02002351       7 (-155678.3 7157033, -1603…   16882. -1.56e5 7.16e6
    # ℹ 3 more variables: u <dbl>, v <dbl>, flow_ID <chr>

### Distance Matrix calculation

Calculate a pairwise distance measure between all flows using their
coordinates.  
You can adjust `alpha` and `beta` to change the weighting of start and
end locations.

``` r
# Compute pairwise flow distances (fd and fds columns)
flows = st_drop_geometry(flows_sf)
distances <- flow_distance(flows, alpha = 1, beta = 1)
```

Convert the long-format distance table to a matrix, which is required
for clustering.

``` r
# Create a distance matrix from the long-form distance data. Choose the column for distances you want to use.
# The 'fds' column is the flow dissimilarity measure, and the 'fd' column is the flow distance measure.
dmat <- distance_matrix(distances, distance_col = "fds")
# check 1st couple of rows columns of the distance matrix
head(dmat[1:2, 1:2])
```

                            E02002330_1-E02002331_2 E02002330_1-E02002334_5
    E02002330_1-E02002331_2               0.0000000               0.9836044
    E02002330_1-E02002334_5               0.9836044               0.0000000

### Clustering

Prepare a weight vector, typically based on a “count” column (number of
trips, etc).  
If your data does not have a “count” column, you can add one with
`flows$count <- 1`. Weights are very handy, otherwise our matrix would
be huge, as we would have to replicate each OD pair n times depending on
the number of observations between them. Unfortunately, there is no out
of the

``` r
# Prepare weights for each flow (here we use the count column)
wvec <- weight_vector(dmat, flows, weight_col = "count")
```

Finally, cluster the flows using DBSCAN.  
Adjust `eps` and `minPts` to control cluster tightness and minimum
cluster size. Cluster composition is greatly affected by these
prameters.

``` r
# Cluster flows using DBSCAN
flows_clustered <- cluster_flows_dbscan(dmat, wvec, flows, eps = 8, minPts = 70)

# View the first few rows of the clustered data
head(flows_clustered, 10)
```

    # A tibble: 10 × 10
       origin    destination count length_m        x        y       u      v flow_ID
       <chr>     <chr>       <dbl>    <dbl>    <dbl>    <dbl>   <dbl>  <dbl> <chr>  
     1 E02002330 E02002331     366    5960. -155678. 7157033. -1.50e5 7.16e6 E02002…
     2 E02002330 E02002334      31   10039. -155678. 7157033. -1.51e5 7.15e6 E02002…
     3 E02002330 E02002335      17   10832. -155678. 7157033. -1.64e5 7.15e6 E02002…
     4 E02002330 E02002349       1   19770. -155678. 7157033. -1.67e5 7.14e6 E02002…
     5 E02002330 E02002351       7   16882. -155678. 7157033. -1.60e5 7.14e6 E02002…
     6 E02002330 E02002358       5   19213. -155678. 7157033. -1.62e5 7.14e6 E02002…
     7 E02002330 E02002359      10   19061. -155678. 7157033. -1.52e5 7.14e6 E02002…
     8 E02002331 E02002330      19    5960. -149890. 7155611. -1.56e5 7.16e6 E02002…
     9 E02002331 E02002334      61    7609. -149890. 7155611. -1.51e5 7.15e6 E02002…
    10 E02002331 E02002335      17   14973. -149890. 7155611. -1.64e5 7.15e6 E02002…
    # ℹ 1 more variable: cluster <int>

``` r
# how many unique clusters were found?
length(unique(flows_clustered$cluster))
```

    [1] 315

``` r
# number of flows in each cluster
flows_clustered |> 
  group_by(cluster) |> 
  summarise(n = n()) |> 
  arrange(desc(n))
```

    # A tibble: 315 × 2
       cluster     n
         <int> <int>
     1       0  6607
     2      33   157
     3      16    61
     4      55    27
     5     262    20
     6      24    19
     7     188    17
     8     162    16
     9      30    14
    10     189    13
    # ℹ 305 more rows

### Visualise

Let’s take a look at the clusters

``` r
# Keep only the biggest clusters for visualisation
flows_clustered = flows_clustered |>
  filter(cluster != 0) |> # these are normally the noisepoints
  group_by(cluster) |>
  mutate(size = n(), 
         count_cluster = sum(count)) |>
  ungroup() |>
  filter(size > 7, # minimum size of cluster
         count_cluster > 100) # minumum number of trips in cluster
```

Add geometry back onto the data

``` r
# Add the geometry back onto the data
flows_clustered = flows_sf |>
  select(flow_ID) |>
  inner_join(flows_clustered, by = "flow_ID") 
```

``` r
# plot
 tm_shape(flows_clustered) +
  tm_lines(lwd = "count",
           col = "cluster",
           palette = "Accent", #YlGn
           #style = "pretty",
           alpha = 1,
           title.col = "Cluster",
           title.lwd = "No. of people",
           scale = 10,
           legend.col.show = FALSE,
           showNA = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = 4,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            # remove panel headers
            #panel.show = FALSE,
            frame = FALSE) -> cluster_results

cluster_results
```

![](README_files/figure-commonmark/unnamed-chunk-13-1.png)

![](man/figures/cluster_results.png)

## Future Work

- [ ] Sensitivity function to show how clustering changes with different
  `eps` and `minPts` values
- [ ] Add more distance matrices (e.g. Frechet distance)
- [ ] Add more clustering algorithms, and use a more efficient data
  structure as a workaround for not being able to use weights with other
  clustering algorithms

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-tao2016spatial" class="csl-entry">

Tao, Ran, and Jean-Claude Thill. 2016. “Spatial Cluster Detection in
Spatial Flow Data.” *Geographical Analysis* 48 (4): 355–72.
<https://doi.org/10.1111/gean.12100>.

</div>

</div>
