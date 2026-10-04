# Add Start/End Coordinates & Flow IDs

Add Start/End Coordinates & Flow IDs

## Usage

``` r
add_xyuv(x)
```

## Arguments

- x:

  sf object of flows

## Value

tibble with x, y, u, v, flow_ID columns

## Examples

``` r
flows <- sf::st_transform(flows_leeds, 3857)
flows <- add_flow_length(flows)
flows <- add_xyuv(flows)
#> Extracting start and end coordinates from flow geometries...
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> Adding x, y, u, v columns to flow data...
#> Assigning unique flow IDs...
```
