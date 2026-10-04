# Aggregate clustered OD flows into representative lines

This function aggregates flows within clusters and creates a single
representative line for each cluster. The start and end coordinates are
computed as weighted averages (weighted by flow counts or another
variable), or simple means if no weights are provided. Each cluster is
represented by one `LINESTRING`.

## Usage

``` r
aggregate_clustered_flows(flows, weight = NULL, crs = sf::st_crs(flows))
```

## Arguments

- flows:

  An `sf` object containing OD flows with coordinates for origins (`x`,
  `y`) and destinations (`u`, `v`), a `cluster` column, and optionally a
  `count` or other weighting variable.

- weight:

  (optional) Name of a column in `flows` to use for weighting. If `NULL`
  (default), unweighted means are used.

- crs:

  Coordinate reference system for the output (default: taken from
  `flows`).

## Value

An `sf` object with one line per cluster, containing:

- `count_total`: total weight (if provided), otherwise number of flows

- `size`: the cluster size (from the input, not recomputed)

- `geometry`: a `LINESTRING` representing the aggregated OD flow

## Examples

``` r
# ----- 1. Basic Usage: A quick, runnable example ---
# This demonstrates the function with minimal, fast data preparation.
flows <- flowcluster::flows_leeds

# Create the required input columns in a single, fast pipeline
flows_clustered <- flows |>
  add_xyuv() |>
  # Manually create 3 dummy clusters for demonstration
  dplyr::mutate(cluster = sample(1:3, size = nrow(flows), replace = TRUE)) |>
  # The function requires a 'size' column, so we add it
  dplyr::group_by(cluster) |>
  dplyr::add_tally(name = "size") |>
  dplyr::ungroup()
#> Extracting start and end coordinates from flow geometries...
#> Adding x, y, u, v columns to flow data...
#> Assigning unique flow IDs...

# Demonstrate the function
flows_agg_w <- aggregate_clustered_flows(flows_clustered, weight = "count")
print(flows_agg_w)
#> Simple feature collection with 3 features and 7 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -1.554589 ymin: 53.80326 xmax: -1.547024 ymax: 53.80754
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 8
#>   cluster count_total  size     x     y     u     v                     geometry
#>     <int>       <dbl> <int> <dbl> <dbl> <dbl> <dbl>             <LINESTRING [°]>
#> 1       1       51378  3479 -1.55  53.8 -1.55  53.8 (-1.547024 53.80493, -1.547…
#> 2       2       53311  3410 -1.55  53.8 -1.55  53.8 (-1.553398 53.80696, -1.550…
#> 3       3       49258  3407 -1.55  53.8 -1.55  53.8 (-1.554589 53.80754, -1.550…

# ----- 2. Detailed Workflow (not run by default) ---
if (FALSE) { # \dontrun{
  # This example shows the ideal end-to-end workflow, from raw data
  # to clustering and finally aggregation. It is not run during checks
  # because the clustering steps are too slow.

  # a) Prepare the data by filtering and adding coordinates
  flows_prep <- flowcluster::flows_leeds |>
    sf::st_transform(3857) |>
    add_flow_length() |>
    filter_by_length(length_min = 5000, length_max = 12000) |>
    add_xyuv()

  # b) Calculate distances and cluster the flows
  distances <- flow_distance(flows_prep, alpha = 1.5, beta = 0.5)
  dmat <- distance_matrix(distances)
  wvec <- weight_vector(dmat, flows_prep, weight_col = "count")
  flows_clustered_real <- cluster_flows_dbscan(dmat, wvec, flows_prep, eps = 8, minPts = 70)

  # c) Filter clusters and add a 'size' column
  flows_clustered_real <- flows_clustered_real |>
    dplyr::filter(cluster != 0) |> # Filter out noise points
    dplyr::group_by(cluster) |>
    dplyr::mutate(size = dplyr::n()) |>
    dplyr::ungroup()

  # d) Now, use the function on the clustered data
  flows_agg_real <- aggregate_clustered_flows(flows_clustered_real, weight = "count")
  print(flows_agg_real)

  # e) Visualize the results
  if (requireNamespace("tmap", quietly = TRUE)) {
    library(tmap)
    # This plot uses modern tmap v4 syntax.
    tm_shape(flows_clustered_real, facet = "cluster") +
      tm_lines(col = "grey50", alpha = 0.5) +
    tm_shape(flows_agg_real) +
      tm_lines(col = "red", lwd = 2) +
    tm_layout(title = "Original Flows (Grey) and Aggregated Flows (Red)")
  }
} # }
```
