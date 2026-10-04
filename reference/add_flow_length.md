# Add Length Column to Flow Data

Also checks that 'origin' and 'destination' columns are present.

## Usage

``` r
add_flow_length(x)
```

## Arguments

- x:

  sf object of flows (LINESTRING, projected CRS)

## Value

sf object with an additional length_m column (od length in meters)

## Examples

``` r
flows <- sf::st_transform(flows_leeds, 3857)
flows <- add_flow_length(flows)
```
