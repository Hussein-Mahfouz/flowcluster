# Sensitivity analysis of DBSCAN parameters for flow clustering.

The function allows you to test different combinations of epsilon and
minPts parameters for clustering flows using DBSCAN. It can be used to
determine what parameter values make sense for your data

## Usage

``` r
dbscan_sensitivity(
  dist_mat,
  flows,
  options_epsilon,
  options_minpts,
  w_vec = NULL
)
```

## Arguments

- dist_mat:

  a precalculated distance matrix between desire lines (output of
  distance_matrix())

- flows:

  the original flows tibble (must contain flow_ID and 'count' column)

- options_epsilon:

  a vector of options for the epsilon parameter

- options_minpts:

  a vector of options for the minPts parameter

- w_vec:

  Optional precomputed weight vector (otherwise computed internally from
  'count' column)

## Value

a tibble with columns: id (to identify eps and minpts), cluster, size
(number of desire lines in cluster), count_sum (total count per cluster)

## Examples

``` r
flows <- sf::st_transform(flows_leeds, 3857)
flows <- head(flows, 1000) # for testing
# Add flow lengths and coordinates
flows <- add_flow_length(flows)
# filter by length
flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#> Flows remaining after filtering: 96 (9.6%)
# Add x, y, u, v coordinates to flows
flows <- add_xyuv(flows)
#> Extracting start and end coordinates from flow geometries...
#> Adding x, y, u, v columns to flow data...
#> Assigning unique flow IDs...
# Calculate distance matrix
distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#> Adding coordinates data back onto the unique pairs ...
dmat <- distance_matrix(distances)
# Generate weight vector
w_vec <- weight_vector(dmat, flows, weight_col = "count")

# Define the parameters for sensitivity analysis
options_epsilon <- seq(1, 10, by = 2)
options_minpts <- seq(10, 100, by = 10)
# # Run the sensitivity analysis
results <- dbscan_sensitivity(
  dist_mat = dmat,
  flows = flows,
  options_epsilon = options_epsilon,
  options_minpts = options_minpts,
  w_vec = w_vec
)
#> running dbscan for option 1 of 50 : eps = 1 | minpts = 10
#> running dbscan for option 2 of 50 : eps = 1 | minpts = 20
#> running dbscan for option 3 of 50 : eps = 1 | minpts = 30
#> running dbscan for option 4 of 50 : eps = 1 | minpts = 40
#> running dbscan for option 5 of 50 : eps = 1 | minpts = 50
#> running dbscan for option 6 of 50 : eps = 1 | minpts = 60
#> running dbscan for option 7 of 50 : eps = 1 | minpts = 70
#> running dbscan for option 8 of 50 : eps = 1 | minpts = 80
#> running dbscan for option 9 of 50 : eps = 1 | minpts = 90
#> running dbscan for option 10 of 50 : eps = 1 | minpts = 100
#> running dbscan for option 11 of 50 : eps = 3 | minpts = 10
#> running dbscan for option 12 of 50 : eps = 3 | minpts = 20
#> running dbscan for option 13 of 50 : eps = 3 | minpts = 30
#> running dbscan for option 14 of 50 : eps = 3 | minpts = 40
#> running dbscan for option 15 of 50 : eps = 3 | minpts = 50
#> running dbscan for option 16 of 50 : eps = 3 | minpts = 60
#> running dbscan for option 17 of 50 : eps = 3 | minpts = 70
#> running dbscan for option 18 of 50 : eps = 3 | minpts = 80
#> running dbscan for option 19 of 50 : eps = 3 | minpts = 90
#> running dbscan for option 20 of 50 : eps = 3 | minpts = 100
#> running dbscan for option 21 of 50 : eps = 5 | minpts = 10
#> running dbscan for option 22 of 50 : eps = 5 | minpts = 20
#> running dbscan for option 23 of 50 : eps = 5 | minpts = 30
#> running dbscan for option 24 of 50 : eps = 5 | minpts = 40
#> running dbscan for option 25 of 50 : eps = 5 | minpts = 50
#> running dbscan for option 26 of 50 : eps = 5 | minpts = 60
#> running dbscan for option 27 of 50 : eps = 5 | minpts = 70
#> running dbscan for option 28 of 50 : eps = 5 | minpts = 80
#> running dbscan for option 29 of 50 : eps = 5 | minpts = 90
#> running dbscan for option 30 of 50 : eps = 5 | minpts = 100
#> running dbscan for option 31 of 50 : eps = 7 | minpts = 10
#> running dbscan for option 32 of 50 : eps = 7 | minpts = 20
#> running dbscan for option 33 of 50 : eps = 7 | minpts = 30
#> running dbscan for option 34 of 50 : eps = 7 | minpts = 40
#> running dbscan for option 35 of 50 : eps = 7 | minpts = 50
#> running dbscan for option 36 of 50 : eps = 7 | minpts = 60
#> running dbscan for option 37 of 50 : eps = 7 | minpts = 70
#> running dbscan for option 38 of 50 : eps = 7 | minpts = 80
#> running dbscan for option 39 of 50 : eps = 7 | minpts = 90
#> running dbscan for option 40 of 50 : eps = 7 | minpts = 100
#> running dbscan for option 41 of 50 : eps = 9 | minpts = 10
#> running dbscan for option 42 of 50 : eps = 9 | minpts = 20
#> running dbscan for option 43 of 50 : eps = 9 | minpts = 30
#> running dbscan for option 44 of 50 : eps = 9 | minpts = 40
#> running dbscan for option 45 of 50 : eps = 9 | minpts = 50
#> running dbscan for option 46 of 50 : eps = 9 | minpts = 60
#> running dbscan for option 47 of 50 : eps = 9 | minpts = 70
#> running dbscan for option 48 of 50 : eps = 9 | minpts = 80
#> running dbscan for option 49 of 50 : eps = 9 | minpts = 90
#> running dbscan for option 50 of 50 : eps = 9 | minpts = 100
```
