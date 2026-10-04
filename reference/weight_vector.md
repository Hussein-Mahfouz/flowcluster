# Generate Weight Vector from Flows

Generate Weight Vector from Flows

## Usage

``` r
weight_vector(dist_mat, x, weight_col = "count")
```

## Arguments

- dist_mat:

  distance matrix

- x:

  flows tibble with flow_ID and weight_col

- weight_col:

  column to use as weights (default = "count")

## Value

numeric weight vector. Each element corresponds to a flow in the
distance matrix, and is used as a weight in the DBSCAN clustering
algorithm.

## Examples

``` r
flows <- sf::st_transform(flows_leeds, 3857)
flows <- head(flows, 100) # for testing
# Add flow lengths and coordinates
flows <- add_flow_length(flows)
flows <- add_xyuv(flows)
#> Extracting start and end coordinates from flow geometries...
#> Adding x, y, u, v columns to flow data...
#> Assigning unique flow IDs...
# Calculate distances
distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#> Adding coordinates data back onto the unique pairs ...
dmat <- distance_matrix(distances)
wvec <- weight_vector(dmat, flows, weight_col = "count")
```
