# Assign Unique IDs to Flows (internal) Internal helper for assigning unique IDs to flows based on spatial columns. Used by `add_xyuv()`

Assign Unique IDs to Flows (internal) Internal helper for assigning
unique IDs to flows based on spatial columns. Used by
[`add_xyuv()`](https://hussein-mahfouz.github.io/flowcluster/reference/add_xyuv.md)

## Usage

``` r
add_flow_ids(x)
```

## Arguments

- x:

  tibble with origin, destination, x, y, u, v columns

## Value

tibble with flow_ID column
