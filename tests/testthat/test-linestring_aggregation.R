test_that("aggregate_clustered_flows: unweighted aggregation works", {
  flows <- flows_leeds |>
    st_transform(3857) |>
    slice(1:200) |>
    add_flow_length() |>
    filter_by_length(1000, 20000) |>
    add_xyuv()
  
  # add dummy clusters and sizes
  flows$cluster <- sample(1:5, nrow(flows), replace = TRUE)
  flows$size <- 1
  
  agg <- aggregate_clustered_flows(flows)
  
  # --- Tests ---
  expect_s3_class(agg, "sf")  # Output is an sf object
  expect_true(all(c("cluster", "count_total", "size") %in% names(agg)))
  expect_true(all(sf::st_geometry_type(agg) == "LINESTRING"))
  expect_equal(nrow(agg), length(unique(flows$cluster)))
  expect_true(all(agg$count_total >= 1))
  expect_true(all(sf::st_is_valid(agg)))  # Geometries are valid
})

test_that("aggregate_clustered_flows: weighted aggregation works", {
  flows <- flows_leeds |>
    st_transform(3857) |>
    slice(1:200) |>
    add_flow_length() |>
    filter_by_length(1000, 20000) |>
    add_xyuv()
  
  # add dummy clusters and sizes
  flows$cluster <- sample(1:3, nrow(flows), replace = TRUE)
  flows$size <- 1
  
  agg <- aggregate_clustered_flows(flows, weight = "count")
  
  expect_s3_class(agg, "sf")
  expect_equal(nrow(agg), length(unique(flows$cluster)))
  expect_true(all(sf::st_is_valid(agg)))
  
  # Check that weighted means are correctly calculated
  manual_sum <- dplyr::summarise(dplyr::group_by(flows, cluster), count_total = sum(count))
  expect_equal(agg$count_total, manual_sum$count_total)
})

test_that("aggregate_clustered_flows: handles missing weights gracefully", {
  flows <- flows_leeds |>
    st_transform(3857) |>
    slice(1:100) |>
    add_flow_length() |>
    filter_by_length(1000, 20000) |>
    add_xyuv()
  
  flows$cluster <- 1
  flows$size <- 1
  
  expect_error(
    aggregate_clustered_flows(flows, weight = "nonexistent"),
    "object 'nonexistent' not found"
  )
})


test_that("aggregate_clustered_flows: calculates weighted means correctly", {
  # Create a simple, predictable data frame
  flows_simple_df <- tibble::tribble(
    ~x, ~y, ~u, ~v, ~cluster, ~size, ~flow,
    1,  2, 10, 20, 1,        2,     1,      # Cluster 1, flow of 1
    3,  6, 30, 60, 1,        2,     3,      # Cluster 1, flow of 3
    5,  5, 50, 50, 2,        1,     10,      # Cluster 2 (single flow)
    2,  4, 20, 40, 3,        1,     5,       # Cluster 3, flow of 5
    5,  7, 70, 80, 3,        1,     15,      # Cluster 3, flow of 15
    4,  8, 40, 80, 3,        1,     10       # Cluster 3, flow of 10
  )
  
  # Create a list of linestrings, one for each row
  linestrings <- purrr::pmap(flows_simple_df, function(x, y, u, v, ...) {
    sf::st_linestring(matrix(c(x, y, u, v), ncol = 2, byrow = TRUE))
  })
  
  # Build the final sf object for the test
  flows_simple <- sf::st_as_sf(
    flows_simple_df,
    geometry = sf::st_sfc(linestrings, crs = 4326)
  )
  
  agg <- aggregate_clustered_flows(flows_simple, weight = "flow")
  
  # --- Manual Calculation for Cluster 1 ---
  # Total weight = 1 + 3 = 4
  # Weighted x = ((1*1) + (3*3)) / 4 = 10 / 4 = 2.5
  # Weighted y = ((1*2) + (3*6)) / 4 = 20 / 4 = 5.0
  # Weighted u = ((1*10) + (3*30)) / 4 = 100 / 4 = 25.0
  # Weighted v = ((1*20) + (3*60)) / 4 = 200 / 4 = 50.0
  
  cluster1_agg <- agg |> dplyr::filter(cluster == 1)
  
  expect_equal(cluster1_agg$count_total, 4)
  expect_equal(cluster1_agg$x, 2.5)
  expect_equal(cluster1_agg$y, 5.0)
  expect_equal(cluster1_agg$u, 25.0)
  expect_equal(cluster1_agg$v, 50.0)
  
  cluster3_agg <- agg |> dplyr::filter(cluster == 3)
  
  expect_equal(cluster3_agg$count_total, 30)  # 5 + 15 + 10
  expect_equal(cluster3_agg$x, ((2*5) + (5*15) + (4*10)) / (5+15+10))  
  expect_equal(cluster3_agg$y, ((4*5) + (7*15) + (8*10)) /  (5+15+10))
  expect_equal(cluster3_agg$u, ((20*5) + (70*15) + (40*10)) /  (5+15+10))
  expect_equal(cluster3_agg$v, ((40*5) + (80*15) + (80*10)) /  (5+15+10))
})
