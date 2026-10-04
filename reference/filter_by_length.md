# Filter Flows by Length

Filter Flows by Length

## Usage

``` r
filter_by_length(x, length_min = 0, length_max = Inf)
```

## Arguments

- x:

  sf object with length_m

- length_min:

  minimum length (default 0)

- length_max:

  maximum length (default Inf)

## Value

filtered sf object. Flows with length_m outside the specified range are
removed.

## Examples

``` r
flows <- sf::st_transform(flows_leeds, 3857)
flows <- add_flow_length(flows)
flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#> Flows remaining after filtering: 3237 (31.44%)
```
