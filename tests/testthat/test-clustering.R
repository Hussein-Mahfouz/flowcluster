library(testthat)
library(dplyr)
library(tibble)
library(sf)

# Load sample flow data and project to metric CRS (e.g., EPSG:3857)
flows <- flows_leeds
flows <- st_transform(flows, 3857)
flows <- flows[1:1000, ]
# Add length (meters) to each flow line
flows <- add_flow_length(flows)
# Filter flows based on length (e.g., between 100 and 10000 meters)
flows <- filter_by_length(flows, length_min = 1000, length_max = 20000)
# Add x, y, u, v coordinates to flows
flows <- add_xyuv(flows)


test_that("flow_distance: alpha/beta validation and output", {
  # Error if alpha/beta not positive or don't sum to 2
  expect_error(flow_distance(flows, alpha = -1, beta = 3), "Alpha and beta must be positive numbers")
  expect_error(flow_distance(flows, alpha = 1, beta = 0.5), "Alpha and beta must sum to 2")

  # Works for valid arguments
  distances <- flow_distance(flows, alpha = 1, beta = 1)
  expect_true(is.data.frame(distances))
  expect_true(all(c("flow_ID_a", "flow_ID_b", "fd", "fds") %in% names(distances)))
  expect_false(any(is.na(distances$fd)))
  expect_false(any(is.na(distances$fds)))
})

test_that("distance_matrix: returns a proper matrix", {
  distances <- flow_distance(flows, alpha = 1, beta = 1)
  mat <- distance_matrix(distances, distance_col = "fds")
  expect_true(is.matrix(mat) || is.data.frame(mat))
  expect_equal(nrow(mat), length(unique(flows$flow_ID)))
  expect_equal(ncol(mat), length(unique(flows$flow_ID)))
})

test_that("weight_vector: returns correct length and values", {
  distances <- flow_distance(flows, alpha = 1, beta = 1)
  mat <- distance_matrix(distances, distance_col = "fds")
  wvec <- weight_vector(mat, flows, weight_col = "count")
  expect_equal(length(wvec), nrow(mat))
  expect_true(all(wvec >= 0)) # Weights should be non-negative
})

test_that("cluster_flows_dbscan: returns clusters", {
  distances <- flow_distance(flows, alpha = 1, beta = 1)
  mat <- distance_matrix(distances, distance_col = "fds")
  wvec <- weight_vector(mat, flows, weight_col = "count")
  # Use small eps/minPts for small sample
  res <- cluster_flows_dbscan(mat, wvec, flows, eps = 0.5, minPts = 1)
  expect_true("cluster" %in% names(res))
  expect_equal(nrow(res), nrow(flows))
  expect_type(res$cluster, "integer")
})

test_that("dbscan_sensitivity: runs without error", {
  distances <- flow_distance(flows, alpha = 1, beta = 1)
  mat <- distance_matrix(distances, distance_col = "fds")
  wvec <- weight_vector(mat, flows, weight_col = "count")

  options_epsilon <- seq(1, 6, by = 2)
  options_minpts <- seq(0, 150, by = 50)

  results <- dbscan_sensitivity(
    dist_mat = mat,
    flows = flows,
    options_epsilon = options_epsilon,
    options_minpts = options_minpts,
    w_vec = wvec
  )
  # check that results is a dataframe and contains expected columns
  expect_true(is.data.frame(results))
  expect_true(all(c("id", "cluster", "size", "count_sum") %in% names(results)))
})
