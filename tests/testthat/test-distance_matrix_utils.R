# test-process_flows.R
library(testthat)
library(sf)
library(dplyr)
library(units)
library(glue)
library(lwgeom)

test_that(".check_flow_columns() works as expected", {
  # Should succeed with correct columns
  df <- data.frame(origin = 1:3, destination = 4:6)
  expect_invisible(.check_flow_columns(df))

  # Should fail if 'origin' missing
  df2 <- data.frame(destination = 1:3)
  expect_error(.check_flow_columns(df2), "Missing required columns: origin")

  # Should fail if 'destination' missing
  df3 <- data.frame(origin = 1:3)
  expect_error(.check_flow_columns(df3), "Missing required columns: destination")

  # Should fail if both missing
  df4 <- data.frame(a = 1:3)
  expect_error(.check_flow_columns(df4), "Missing required columns: origin, destination")
})

test_that("add_flow_length() adds length_m and checks CRS", {
  flows <- flows_leeds
  flows_proj <- st_transform(flows, 3857)
  result <- add_flow_length(flows_proj)

  expect_true("length_m" %in% names(result))
  expect_type(result$length_m, "double")
  # check if values are greater than 0
  expect_gt(mean(result$length_m), 0)

  flows_longlat <- st_transform(flows, 4326)
  expect_error(add_flow_length(flows_longlat), "CRS is geographic")
})

test_that("filter_by_length() filters correctly", {
  flows <- st_transform(flows_leeds, 3857)
  flows <- add_flow_length(flows)
  n_orig <- nrow(flows)

  filtered <- filter_by_length(flows, length_min = 1000, length_max = 5000)
  expect_true(all(filtered$length_m >= 1000 & filtered$length_m <= 5000))

  # Edge case: all filtered out
  filtered_empty <- filter_by_length(flows, length_min = 1e9)
  expect_equal(nrow(filtered_empty), 0)
})


test_that("filter_by_length filters flows correctly", {
  # Prepare data: project and add length
  flows <- flows_leeds
  flows <- sf::st_transform(flows, 3857)
  flows$length_m <- units::drop_units(sf::st_length(flows))

  # Normal filtering
  filtered <- filter_by_length(flows, length_min = 1000, length_max = 5000)
  expect_true(all(filtered$length_m >= 1000 & filtered$length_m <= 5000))
  expect_true(nrow(filtered) <= nrow(flows))

  # Filtering with a range that excludes all rows returns 0
  filtered_none <- filter_by_length(flows, length_min = 1e10, length_max = 1e11)
  expect_equal(nrow(filtered_none), 0)
})

test_that("filter_by_length handles invalid arguments", {
  flows <- flows_leeds
  flows <- sf::st_transform(flows, 3857)
  flows$length_m <- units::drop_units(sf::st_length(flows))

  # length_min greater than length_max
  expect_error(
    filter_by_length(flows, length_min = 1000, length_max = 500),
    "length_min cannot be greater than length_max."
  )

  # Negative length_min
  expect_error(
    filter_by_length(flows, length_min = -10, length_max = 1000),
    "Both length_min and length_max must be numeric positive values."
  )

  # Negative length_max
  expect_error(
    filter_by_length(flows, length_min = 0, length_max = -1000),
    "Both length_min and length_max must be numeric positive values."
  )

  # Non-numeric arguments
  expect_error(
    filter_by_length(flows, length_min = "a", length_max = 1000),
    "Both length_min and length_max must be numeric positive values."
  )
  expect_error(
    filter_by_length(flows, length_min = 0, length_max = "b"),
    "Both length_min and length_max must be numeric positive values."
  )
})

test_that("filter_by_length returns correct columns and type", {
  flows <- flows_leeds
  flows <- sf::st_transform(flows, 3857)
  flows$length_m <- units::drop_units(sf::st_length(flows))

  filtered <- filter_by_length(flows, length_min = 1000, length_max = 5000)
  expect_true("length_m" %in% names(filtered))
  expect_true(nrow(filtered) <= nrow(flows))
})




test_that("add_xyuv() adds x, y, u, v  columns", {
  flows <- st_transform(flows_leeds, 3857)
  flows <- add_flow_length(flows)
  result <- add_xyuv(flows)
  expect_true(all(c("x", "y", "u", "v") %in% names(result)))
  expect_equal(nrow(result), nrow(flows))
  expect_false(any(is.na(result$x)))
  expect_false(any(is.na(result$u)))
})

test_that("add_flow_ids() assigns unique flow_IDs", {
  # Create minimal example with required columns
  df <- tibble::tibble(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    x = c(1, 1, 2), y = c(2, 2, 3),
    u = c(3, 4, 4), v = c(4, 5, 5)
  )
  result <- add_flow_ids(df)
  expect_true("flow_ID" %in% names(result))
  expect_equal(length(unique(result$flow_ID)), nrow(result))
  expect_false(any(is.na(result$flow_ID)))
  # check actual values of flow_ID
  expect_equal(result$flow_ID, c("A_1-B_1", "A_1-C_2", "B_2-C_2"))
})
