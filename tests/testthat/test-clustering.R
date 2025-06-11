library(testthat)
library(dplyr)
library(tibble)

# Use a small sample for speed
flows <- flows_leeds %>%
  sf::st_transform(3857) %>%
  add_flow_length() %>%
  add_xyuv() %>%
  head(1000)


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
