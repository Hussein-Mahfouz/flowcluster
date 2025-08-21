#' Aggregate clustered OD flows into representative lines
#'
#' This function aggregates flows within clusters and creates a single
#' representative line for each cluster. The start and end coordinates are
#' computed as weighted averages (weighted by flow counts or another variable),
#' or simple means if no weights are provided. Each cluster is represented
#' by one `LINESTRING`.
#'
#' @param flows An `sf` object containing OD flows with coordinates for origins
#'   (`x`, `y`) and destinations (`u`, `v`), a `cluster` column, and optionally
#'   a `count` or other weighting variable.
#' @param weight (optional) Name of a column in `flows` to use for weighting.
#'   If `NULL` (default), unweighted means are used.
#' @param crs Coordinate reference system for the output (default: taken from
#'   `flows`).
#'
#' @return An `sf` object with one line per cluster, containing:
#' \itemize{
#'   \item `count_total`: total weight (if provided), otherwise number of flows
#'   \item `size`: the cluster size (from the input, not recomputed)
#'   \item `geometry`: a `LINESTRING` representing the aggregated OD flow
#' }
#'
#' @examples
#' # ----- 1. Basic Usage: A quick, runnable example ---
#' # This demonstrates the function with minimal, fast data preparation.
#' flows <- flowcluster::flows_leeds
#'
#' # Create the required input columns in a single, fast pipeline
#' flows_clustered <- flows |>
#'   add_xyuv() |>
#'   # Manually create 3 dummy clusters for demonstration
#'   dplyr::mutate(cluster = sample(1:3, size = nrow(flows), replace = TRUE)) |>
#'   # The function requires a 'size' column, so we add it
#'   dplyr::group_by(cluster) |>
#'   dplyr::add_tally(name = "size") |>
#'   dplyr::ungroup()
#'
#' # Demonstrate the function
#' flows_agg_w <- aggregate_clustered_flows(flows_clustered, weight = "count")
#' print(flows_agg_w)
#'
#' # ----- 2. Detailed Workflow (not run by default) ---
#' \dontrun{
#'   # This example shows the ideal end-to-end workflow, from raw data
#'   # to clustering and finally aggregation. It is not run during checks
#'   # because the clustering steps are too slow.
#'
#'   # a) Prepare the data by filtering and adding coordinates
#'   flows_prep <- flowcluster::flows_leeds |>
#'     sf::st_transform(3857) |>
#'     add_flow_length() |>
#'     filter_by_length(length_min = 5000, length_max = 12000) |>
#'     add_xyuv()
#'
#'   # b) Calculate distances and cluster the flows
#'   distances <- flow_distance(flows_prep, alpha = 1.5, beta = 0.5)
#'   dmat <- distance_matrix(distances)
#'   wvec <- weight_vector(dmat, flows_prep, weight_col = "count")
#'   flows_clustered_real <- cluster_flows_dbscan(dmat, wvec, flows_prep, eps = 8, minPts = 70)
#'
#'   # c) Filter clusters and add a 'size' column
#'   flows_clustered_real <- flows_clustered_real |>
#'     dplyr::filter(cluster != 0) |> # Filter out noise points
#'     dplyr::group_by(cluster) |>
#'     dplyr::mutate(size = dplyr::n()) |>
#'     dplyr::ungroup()
#'
#'   # d) Now, use the function on the clustered data
#'   flows_agg_real <- aggregate_clustered_flows(flows_clustered_real, weight = "count")
#'   print(flows_agg_real)
#'
#'   # e) Visualize the results
#'   if (requireNamespace("tmap", quietly = TRUE)) {
#'     library(tmap)
#'     # This plot uses modern tmap v4 syntax.
#'     tm_shape(flows_clustered_real, facet = "cluster") +
#'       tm_lines(col = "grey50", alpha = 0.5) +
#'     tm_shape(flows_agg_real) +
#'       tm_lines(col = "red", lwd = 2) +
#'     tm_layout(title = "Original Flows (Grey) and Aggregated Flows (Red)")
#'   }
#' }
#' @export
aggregate_clustered_flows <- function(flows, weight = NULL, crs = sf::st_crs(flows)) {
  
  # Store the original CRS from the input data
  input_crs <- sf::st_crs(flows)
  
  if (!is.null(weight)) {
    weight <- rlang::ensym(weight)
    
    # Weighted aggregation
    flows_aggregated <- flows |>
      sf::st_drop_geometry() |>
      dplyr::group_by(.data$cluster) |>
      dplyr::summarise(
        count_total = sum(!!weight, na.rm = TRUE),
        size = dplyr::first(.data$size),
        x = stats::weighted.mean(.data$x, !!weight, na.rm = TRUE),
        y = stats::weighted.mean(.data$y, !!weight, na.rm = TRUE),
        u = stats::weighted.mean(.data$u, !!weight, na.rm = TRUE),
        v = stats::weighted.mean(.data$v, !!weight, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    # Unweighted aggregation
    flows_aggregated <- flows |>
      sf::st_drop_geometry() |>
      dplyr::group_by(.data$cluster) |>
      dplyr::summarise(
        count_total = dplyr::n(),
        size = dplyr::first(.data$size),
        x = mean(.data$x, na.rm = TRUE),
        y = mean(.data$y, na.rm = TRUE),
        u = mean(.data$u, na.rm = TRUE),
        v = mean(.data$v, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Create LINESTRING geometries from the aggregated coordinates
  linestrings <- purrr::pmap(
    dplyr::select(flows_aggregated, dplyr::all_of(c("x", "y", "u", "v"))),
    function(x, y, u, v) {
      sf::st_linestring(matrix(c(x, y, u, v), ncol = 2, byrow = TRUE))
  }
  )
  
  # 1. Create the new sf object using the ORIGINAL input CRS
  # The coordinate values were calculated in this system, so they must be tagged with it.
  flows_agg_sf <- sf::st_as_sf(
    flows_aggregated,
    geometry = sf::st_sfc(linestrings, crs = input_crs)
  )
  
  # 2. Conditionally transform to the target CRS if it's different
  # The 'crs' variable holds the user's desired output CRS.
  if (input_crs != crs) {
    flows_agg_sf <- sf::st_transform(flows_agg_sf, crs = crs)
  }
  
  # Return the final, correctly projected sf object
  return(flows_agg_sf)
}

