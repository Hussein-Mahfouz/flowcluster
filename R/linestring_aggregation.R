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
#' # Weighted aggregation (by flow counts)
#' flows_agg_w <- aggregate_clustered_flows(flows_clustered, weight = "count")
#'
#' # Unweighted aggregation
#' flows_agg_uw <- aggregate_clustered_flows(flows_clustered)
#'
#' library(tmap)
#' tm_shape(flows_agg_w) + tm_lines(lwd = "count_total", scale = 9)
#'
#' @export
aggregate_clustered_flows <- function(flows, weight = NULL, crs = sf::st_crs(flows)) {
  
  # Store the original CRS from the input data
  input_crs <- sf::st_crs(flows)
  
  if (!is.null(weight)) {
    weight <- rlang::ensym(weight)
    
    # Weighted aggregation
    flows_aggregated <- flows |>
      sf::st_drop_geometry() |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        count_total = sum(!!weight, na.rm = TRUE),
        size = dplyr::first(size),
        x = stats::weighted.mean(x, !!weight, na.rm = TRUE),
        y = stats::weighted.mean(y, !!weight, na.rm = TRUE),
        u = stats::weighted.mean(u, !!weight, na.rm = TRUE),
        v = stats::weighted.mean(v, !!weight, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    # Unweighted aggregation
    flows_aggregated <- flows |>
      sf::st_drop_geometry() |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        count_total = dplyr::n(),
        size = dplyr::first(size),
        x = mean(x, na.rm = TRUE),
        y = mean(y, na.rm = TRUE),
        u = mean(u, na.rm = TRUE),
        v = mean(v, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Create LINESTRING geometries from the aggregated coordinates
  linestrings <- purrr::pmap(dplyr::select(flows_aggregated, x, y, u, v), function(x, y, u, v) {
    sf::st_linestring(matrix(c(x, y, u, v), ncol = 2, byrow = TRUE))
  })
  
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
