# Calculate Flow Distance and Dissimilarity

This function calculates flow distance and dissimilarity measures
between all pairs of flows based on the method described in
@tao2016spatial.

## Usage

``` r
flow_distance(x, alpha = 1, beta = 1)
```

## Arguments

- x:

  tibble with flow_ID, x, y, u, v, length_m

- alpha:

  numeric, origin weight

- beta:

  numeric, destination weight

## Value

tibble of all OD pairs with fd, fds columns

## References

Tao, R., Thill, J.-C., 2016. Spatial cluster detection in spatial flow
data. Geographical Analysis 48, 355–372.
https://doi.org/10.1111/gean.12100

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
```
