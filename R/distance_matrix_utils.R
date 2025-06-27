#' Check that required columns are present in the flow data
#'
#' @param x A data.frame or tibble to check.
#' @noRd
.check_flow_columns <- function(x) {
  required <- c("origin", "destination")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}


#' Add Length Column to Flow Data
#' 
#' Also checks that 'origin' and 'destination' columns are present.
#' @param x sf object of flows (LINESTRING, projected CRS)
#' @return sf object with an additional length_m column (od length in meters)
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows <- add_flow_length(flows)
#' @export
add_flow_length <- function(x) {
  .check_flow_columns(x)
  if (sf::st_is_longlat(x)) {
    stop("CRS is geographic. Please provide a projected (metric) CRS, e.g., EPSG:3857.")
  }
  x$length_m <- units::drop_units(sf::st_length(x))
  x
}

#' Filter Flows by Length
#' @param x sf object with length_m
#' @param length_min minimum length (default 0)
#' @param length_max maximum length (default Inf)
#' @return filtered sf object. Flows with length_m outside the specified range are removed.
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows <- add_flow_length(flows)
#' flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#' @export
filter_by_length <- function(x, length_min = 0, length_max = Inf) {
  # check that both length_min and length_max are numeric values
  if (!is.numeric(length_min) || !is.numeric(length_max) || 
      length_min < 0 || length_max < 0) {
    stop("Both length_min and length_max must be numeric positive values.")
  }
  # check that length_min is not greater than length_max
  if (length_min > length_max) {
    stop("length_min cannot be greater than length_max.")
  }
  # check that length_min and length_max are positive
  if (length_min < 0 || length_max < 0) {
    stop("Both length_min and length_max must be positive values.")
  }
  # No flows should have length 0. If length_min is 0, we send a message and filter 
  # out any flows with length 0.
  if (length_min == 0) {
    message("Filtering out flows with length 0...")
    x <- dplyr::filter(x, length_m > 0)
  }
  # Filter flows by user specified length range
  x_filtered <- dplyr::filter(x, length_m >= length_min & length_m <= length_max)
  message(
    glue::glue("Flows remaining after filtering: {nrow(x_filtered)} (",
               round(nrow(x_filtered) / nrow(x) * 100, 2), "%)")
  )
  x_filtered
}

#' Add Start/End Coordinates & Flow IDs
#' @param x sf object of flows
#' @return tibble with x, y, u, v, flow_ID columns
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows <- add_flow_length(flows)
#' flows <- add_xyuv(flows)
#' @export
add_xyuv <- function(x) {
  # Ensure input is sf
  stopifnot(inherits(x, "sf"))
  message("Extracting start and end coordinates from flow geometries...")
  # Extract start and end points as sfc
  start_points <- lwgeom::st_startpoint(x$geometry)
  end_points   <- lwgeom::st_endpoint(x$geometry)
  
  # Get coordinates
  start_coords <- sf::st_coordinates(start_points)
  end_coords   <- sf::st_coordinates(end_points)
  
  # Add to data frame
  message("Adding x, y, u, v columns to flow data...")
  x$x <- start_coords[, "X"]
  x$y <- start_coords[, "Y"]
  x$u <- end_coords[, "X"]
  x$v <- end_coords[, "Y"]
  # Add flow IDs
  message("Assigning unique flow IDs...")
  add_flow_ids(x)
}

#' Assign Unique IDs to Flows (internal)

#' Internal helper for assigning unique IDs to flows based on spatial columns. Used by \code{add_xyuv()}
#' @param x tibble with origin, destination, x, y, u, v columns
#' @return tibble with flow_ID column
#' @keywords internal
add_flow_ids <- function(x) {
  x |>
    dplyr::group_by(.data$origin, .data$x, .data$y) |>
    dplyr::mutate(origin_id = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$destination, .data$u, .data$v) |>
    dplyr::mutate(dest_id = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::mutate(flow_ID = paste0(.data$origin, "_", origin_id, "-", .data$destination, "_", dest_id)) |>
    dplyr::select(-origin_id, -dest_id)
}
