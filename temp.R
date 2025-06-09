devtools::load_all()
flows_leeds






add_od_length <- function(od_sf) {
  # check crs
  if (sf::st_is_longlat(od_sf)) {
    stop("The CRS is geographic (longitude/latitude). Please provide a projected (metric) CRS, e.g., EPSG:3857.")
  } else {
    message("CRS is projected. Proceeding...")
  }
  # Add length in metres
  message("Calculating lengths of the flow lines in metres.")
  od_sf$length_m <- units::drop_units(sf::st_length(od_sf))
  return(od_sf)
}

filter_by_length <- function(od_sf, distance_threshold_min = 0, distance_threshold_max = Inf) {
  # Filter flows by length
  message("Filtering flows by length.")
  od_sf_filtered <- od_sf |>
    dplyr::filter(length_m >= distance_threshold_min & length_m <= distance_threshold_max)
  # how many flows are remaining (absolute and % of original pre filtering)
  message(glue::glue("Number of flows remaining after filtering: {nrow(od_sf_filtered)} ({round(nrow(od_sf_filtered) / nrow(flows_leeds) * 100, 2)}%)"))
  return(od_sf_filtered)
}



add_xyuv <- function(od_sf) {
  # Adding start and end coordinates to the flow data
  message("Adding start and end coordinates to the flow data.")
  xyuv = od_sf |>
    dplyr::mutate(
      x = dplyr::as_tibble(sf::st_coordinates(lwgeom::st_startpoint(od_sf)))$X,
      y = dplyr::as_tibble(sf::st_coordinates(lwgeom::st_startpoint(od_sf)))$Y,
      u = dplyr::as_tibble(sf::st_coordinates(lwgeom::st_endpoint(od_sf)))$X,
      v = dplyr::as_tibble(sf::st_coordinates(lwgeom::st_endpoint(od_sf)))$Y)# |>
    #sf::st_drop_geometry()
  # Add flow IDs
  message("Assigning unique flow IDs based on origin and destination coordinates.")
  xyuv = add_flow_ids(xyuv)
  # # Combine with original sf object
  # message("Combining start and end coordinates with the original flow data.")
  # dplyr::bind_cols(sf::st_drop_geometry(od_sf), xyuv)
}


add_flow_ids <- function(coords) {
  coords <- coords |>
    dplyr::group_by(origin, x, y) |>
    dplyr::mutate(O_ID = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::group_by(destination, u, v) |>
    dplyr::mutate(D_ID = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::mutate(flow_ID = paste0(origin, "_", O_ID, "-", destination, "_", D_ID)) |>
    dplyr::select(-c(O_ID, D_ID))
  
  return(coords)
}

flows_leeds_3857 = sf::st_transform(flows_leeds, crs = 3857)
x = add_od_length(flows_leeds_3857)

x1 = filter_by_length(x, distance_threshold_min = 5000, distance_threshold_max = 12000)

x2 = add_xyuv(x1)







#' Compute Flow Distance and Dissimilarity for All OD Pairs
#'
#' @param flows A data.frame or tibble with columns: flow_ID, x, y, u, v, distance_m
#' @param alpha Numeric. Weight for origin (default 1).
#' @param beta Numeric. Weight for destination (default 1).
#' @return A tibble with all OD pairs and columns: flow_ID_a, flow_ID_b, x_i, ..., distance_m_j, fd, fds
#' @export
flow_distance <- function(flows, alpha = 1, beta = 1) {
  # Create all unique pairs of flows
  flows_grid <- tidyr::expand_grid(flow_ID_a = flows$flow_ID,
                                   flow_ID_b = flows$flow_ID)
  
  # Add coordinate data for flow a
  flows_grid <- dplyr::inner_join(
    flows_grid,
    dplyr::select(flows, flow_ID, x, y, u, v, length_m),
    by = c("flow_ID_a" = "flow_ID")
  )
  flows_grid <- dplyr::rename_with(
    flows_grid,
    .fn = ~ paste0(.x, "_i"),
    .cols = c("x", "y", "u", "v", "length_m")
  )
  
  # Add coordinate data for flow b
  flows_grid <- dplyr::inner_join(
    flows_grid,
    dplyr::select(flows, flow_ID, x, y, u, v, length_m),
    by = c("flow_ID_b" = "flow_ID")
  )
  flows_grid <- dplyr::rename_with(
    flows_grid,
    .fn = ~ paste0(.x, "_j"),
    .cols = c("x", "y", "u", "v", "length_m")
  )
  
  # Calculate flow distance and dissimilarity
  flows_grid <- dplyr::mutate(
    flows_grid,
    fd = sqrt(
      alpha * ((.data$x_i - .data$x_j)^2 + (.data$y_i - .data$y_j)^2) +
        beta * ((.data$u_i - .data$u_j)^2 + (.data$v_i - .data$v_j)^2)
    ),
    fds = sqrt(
      (alpha * ((.data$x_i - .data$x_j)^2 + (.data$y_i - .data$y_j)^2) +
         beta * ((.data$u_i - .data$u_j)^2 + (.data$v_i - .data$v_j)^2)) /
        (.data$length_m_i * .data$length_m_j)
    )
  )
  
  flows_grid
}



# apply the function
x3 = flow_distance(x2, alpha = 1, beta = 1)


#' Convert long-format distance tibble into a distance matrix
#'
#' @param distances A tibble with columns `flow_ID_a`, `flow_ID_b`, and a distance column (e.g., `fds`)
#' @param distance_col The name of the distance column to pivot (default: "fds")
#' @return A distance matrix (tibble with rownames as `flow_ID_a`)
#' @export
distance_matrix <- function(distances, distance_col = "fds") {
  distances |>
    dplyr::select(flow_ID_a, flow_ID_b, all_of(distance_col)) |>
    tidyr::pivot_wider(names_from = flow_ID_b, values_from = all_of(distance_col)) |>
    tibble::column_to_rownames(var = "flow_ID_a")
}


# --- step 3: Convert df to a distance matrix
x4 <- distance_matrix(x3, distance_col = "fds")


#' Generate Weight Vector from Distance Matrix and OD Flow Data
#'
#' @param dist_mat A distance matrix with rownames as flow_IDs.
#' @param flows A data.frame/tibble containing `flow_ID` and `commute_all`.
#' @param weight_col Name of the column in `flows` to use as weights (default = "commute_all").
#'
#' @return A numeric vector of weights (same order as distance matrix rows).
#' @export
weight_vector <- function(dist_mat, flows, weight_col) {
  flows |>
    dplyr::select(flow_ID, tidyselect::all_of(weight_col)) |>
    dplyr::inner_join(
      tibble::tibble(flow_ID = rownames(dist_mat)),
      by = "flow_ID"
    ) |>
    dplyr::pull(tidyselect::all_of(weight_col))
}


x5 <- weight_vector(x4, x2, weight_col = "count")





#' Cluster OD Flows Using DBSCAN with Precomputed Distance Matrix
#'
#' @param dist_mat A precomputed distance matrix (symmetric, rownames = flow_ID).
#' @param w_vec A numeric weight vector (same order as dist_mat rows).
#' @param flows An sf object or tibble containing flow_ID and geometry (e.g., od_demand_jittered).
#' @param eps DBSCAN eps parameter (neighbourhood radius).
#' @param minPts Minimum number of points to form a dense region.
#' @param borderPoints Logical. Whether to assign border points to clusters (default = TRUE).
#'
#' @return An `sf` object with `cluster` column added.
#' @export
cluster_flows_dbscan <- function(dist_mat,
                                 w_vec,
                                 flows,
                                 eps,
                                 minPts) {
  # Run DBSCAN
  clustering <- dbscan::dbscan(
    x = dist_mat,
    eps = eps,
    minPts = minPts,
    weights = w_vec
    )
  
  # Attach cluster labels to flow_IDs
  cluster_df <- tibble::tibble(
    flow_ID = rownames(dist_mat),
    cluster = clustering$cluster
  )
  
  # Join back to flow geometry
  dplyr::inner_join(flows, cluster_df, by = "flow_ID")
}


# Apply clustering function
x6 <- cluster_flows_dbscan(
  dist_mat = x4,
  w_vec = x5,
  flows = x2,
  eps = 8,
  minPts = 70
  )

# Tests 

x7 = sf::st_as_sf(x6)
plot(x7["cluster"])

# Keep only clusters with more than 10 rows
x8 = x7 |>
  dplyr::group_by(cluster) |>
  dplyr::filter(dplyr::n() > 20, cluster !=0) |>
  dplyr::ungroup()

plot(x8["cluster"])
hist(x8$cluster)



# check crs
# 
