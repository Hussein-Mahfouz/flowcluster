# Convert Long-Format Distance Tibble to Matrix

Convert Long-Format Distance Tibble to Matrix

## Usage

``` r
distance_matrix(distances, distance_col = "fds")
```

## Arguments

- distances:

  tibble with columns flow_ID_a, flow_ID_b, and distance

- distance_col:

  column name for distance (default "fds")

## Value

distance matrix (tibble with rownames). The matrix has flow_ID_a as
rownames and flow_ID_b as column names. This function converts the
output of
[`flow_distance()`](https://hussein-mahfouz.github.io/flowcluster/reference/flow_distance.md)
into a format suitable for the
[dbscan](https://rdrr.io/pkg/dbscan/man/dbscan.html) clustering
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
```
