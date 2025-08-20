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
#' # ----- 1. Prepare the data
#' 
#' flows <- sf::st_transform(flows_leeds, 3857)
#' # Add flow lengths and coordinates
#' flows <- add_flow_length(flows)
#' # filter by length
#' flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#' flows <- add_xyuv(flows)
#' # Calculate distances
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' dmat <- distance_matrix(distances)
#' wvec <- weight_vector(dmat, flows, weight_col = "count")
#' 
#' # Cluster flows using DBSCAN
#' flows_clustered <- cluster_flows_dbscan(dmat, wvec, flows, eps = 8, minPts = 70)
#' 
#' # Filter out noise points and small clusters. Calculate size and count per cluster
#' flows_clustered <- flows_clustered |>
#'   dplyr::filter(cluster != 0) |> # these are normally the noisepoints
#'   dplyr::group_by(cluster) |>
#'   dplyr::mutate(size = dplyr::n(),
#'          count_cluster = sum(count)) |>
#'   dplyr::ungroup() |>
#'   # Filter out small clusters
#'   dplyr::filter(size > 7, # minimum size of cluster
#'                 count_cluster > 100) # minumum number of trips in cluster
#'          
#' # ----- 2. Aggregation code
#' 
#' # Weighted aggregation (by flow counts)
#' flows_agg_w <- aggregate_clustered_flows(flows_clustered, weight = "count")
#' head(flows_agg_w)
#' 
#' # Unweighted aggregation
#' flows_agg_uw <- aggregate_clustered_flows(flows_clustered)
#' head(flows_agg_uw)
#' 
#' # ----- 3. Visualize the output ---
#' if (requireNamespace("tmap", quietly = TRUE)) {
#'   library(tmap)
#'
#' tm_shape(flows_clustered) +
#'   tm_lines(
#'   lwd = "count",
#'   col = "grey30",
#'   alpha = 0.7,
#'   title.lwd = "No. of people (Original flows)",
#'   scale = 10,
#'   legend.col.show = FALSE,
#'   showNA = FALSE) +
#' tm_facets(
#'   by = "cluster",
#'   free.coords = FALSE,
#'   nrow = 4,
#'   showNA = FALSE) +
#' tm_shape(flows_agg_w) +
#' tm_lines(
#'   lwd = "count_total",
#'   col = "red",
#'   palette = "Accent", # YlGn
#'   alpha = 1,
#'   title.col = "Cluster",
#'   title.lwd = "No. of people (Representative linestring)",
#'   scale = 10,
#'   legend.outside = TRUE,
#'   legend.outside.position = "bottom") +
#'  tm_facets(
#'    by = "cluster",
#'    free.coords = FALSE,
#'    nrow = 4,
#'    showNA = FALSE) +
#'  tm_layout(
#'    fontfamily = "Georgia",
#'    main.title = "Aggregating flows to representative linestrings per cluster",
#'    main.title.size = 1.1,
#'    main.title.color = "azure4",
#'    main.title.position = "left",
#'    legend.outside = TRUE,
#'    legend.outside.position = "bottom",
#'    legend.stack = "horizontal",
#'   ) 
#'  }
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
