# Cluster Flows using DBSCAN

See [dbscan](https://rdrr.io/pkg/dbscan/man/dbscan.html) for details on
the DBSCAN algorithm.

## Usage

``` r
cluster_flows_dbscan(dist_mat, w_vec, x, eps, minPts)
```

## Arguments

- dist_mat:

  distance matrix

- w_vec:

  weight vector

- x:

  flows tibble with flow_ID

- eps:

  DBSCAN epsilon parameter

- minPts:

  DBSCAN minPts parameter

## Value

flows tibble with an additional cluster column

## Examples

``` r
flows <- sf::st_transform(flows_leeds, 3857)
flows <- head(flows, 100) # for testing
# Add flow lengths and coordinates
flows <- add_flow_length(flows)
# filter by length
flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#> Flows remaining after filtering: 5 (5%)
flows <- add_xyuv(flows)
#> Extracting start and end coordinates from flow geometries...
#> Adding x, y, u, v columns to flow data...
#> Assigning unique flow IDs...
# Calculate distances
distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#> Adding coordinates data back onto the unique pairs ...
dmat <- distance_matrix(distances)
wvec <- weight_vector(dmat, flows, weight_col = "count")
clustered <- cluster_flows_dbscan(dmat, wvec, flows, eps = 8, minPts = 70)
```
