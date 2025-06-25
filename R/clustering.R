#' Calculate Flow Distance and Dissimilarity
#'
#' This function calculates flow distance and dissimilarity measures between all
#' pairs of flows based on the method described in @tao2016spatial.
#' @param x tibble with flow_ID, x, y, u, v, length_m
#' @param alpha numeric, origin weight
#' @param beta numeric, destination weight
#' @return tibble of all OD pairs with fd, fds columns
#' @references
#' Tao, R., Thill, J.-C., 2016. Spatial cluster detection in spatial flow data. Geographical
#' Analysis 48, 355–372. https://doi.org/10.1111/gean.12100
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows = head(flows, 100) # for testing
#' # Add flow lengths and coordinates
#' flows <- add_flow_length(flows)
#' flows <- add_xyuv(flows)
#' # Calculate distances
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' @export
flow_distance <- function(x, alpha = 1, beta = 1) {
  # check that alpha and beta are positive numbers, and that they add up to 2
  if (!is.numeric(alpha) || !is.numeric(beta) || alpha <= 0 || beta <= 0) {
    stop("Alpha and beta must be positive numbers.")
  }
  if (abs(alpha + beta - 2) > 1e-6) {
    stop("Alpha and beta must sum to 2.")
  }
  # create combination pairs of all flows
  grid <- tidyr::expand_grid(flow_ID_a = x$flow_ID, 
                             flow_ID_b = x$flow_ID)
  message("Adding coordinates data back onto the unique pairs ...")
  # add the coordinate data
  grid <- grid |>
    # --- add flow_a coordinates
    dplyr::inner_join(x |> 
                        dplyr::select(.data$flow_ID, .data$x, .data$y, .data$u, .data$v, .data$length_m),
                      by = c("flow_ID_a" = "flow_ID")) |>
    dplyr::rename_with(~paste0(.x, "_i"), c("x", "y", "u", "v", "length_m")) |>
    # --- add flow_b coordinates
    dplyr::inner_join(x |>
                        dplyr::select(.data$x, .data$flow_ID, .data$x, .data$y, .data$u, .data$v, .data$length_m),
                      by = c("flow_ID_b" = "flow_ID")) |>
    dplyr::rename_with(~paste0(.x, "_j"), c("x", "y", "u", "v", "length_m")) |>
    dplyr::mutate(
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
  grid
}

#' Convert Long-Format Distance Tibble to Matrix
#' @param distances tibble with columns flow_ID_a, flow_ID_b, and distance
#' @param distance_col column name for distance (default "fds")
#' @return distance matrix (tibble with rownames)
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows = head(flows, 100) # for testing
#' # Add flow lengths and coordinates
#' flows <- add_flow_length(flows)
#' flows <- add_xyuv(flows)
#' # Calculate distances
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' dmat <- distance_matrix(distances)
#' @export
distance_matrix <- function(distances, distance_col = "fds") {
  distances |>
    dplyr::select(.data$flow_ID_a, .data$flow_ID_b, tidyselect::all_of(distance_col)) |>
    tidyr::pivot_wider(names_from = .data$flow_ID_b, values_from = tidyselect::all_of(distance_col)) |>
    tibble::column_to_rownames(var = "flow_ID_a")
}

#' Generate Weight Vector from Flows
#' @param dist_mat distance matrix
#' @param x flows tibble with flow_ID and weight_col
#' @param weight_col column to use as weights (default = "count")
#' @return numeric weight vector
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows = head(flows, 100) # for testing
#' # Add flow lengths and coordinates
#' flows <- add_flow_length(flows)
#' flows <- add_xyuv(flows)
#' # Calculate distances
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' dmat <- distance_matrix(distances)
#' wvec <- weight_vector(dmat, flows, weight_col = "count")
#' @export
weight_vector <- function(dist_mat, x, weight_col = "count") {
  x |>
    dplyr::select(.data$flow_ID, tidyselect::all_of(weight_col)) |>
    dplyr::inner_join(
      tibble::tibble(flow_ID = rownames(dist_mat)),
      by = "flow_ID"
    ) |>
    dplyr::pull(tidyselect::all_of(weight_col))
}

#' Cluster Flows using DBSCAN
#' 
#' See \link[dbscan]{dbscan} for details on the DBSCAN algorithm.
#' 
#' @param dist_mat distance matrix
#' @param w_vec weight vector
#' @param x flows tibble with flow_ID
#' @param eps DBSCAN epsilon parameter
#' @param minPts DBSCAN minPts parameter
#' @return flows tibble with cluster column
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows = head(flows, 100) # for testing
#' # Add flow lengths and coordinates
#' flows <- add_flow_length(flows)
#' # filter by length
#' flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#' flows <- add_xyuv(flows)
#' # Calculate distances
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' dmat <- distance_matrix(distances)
#' wvec <- weight_vector(dmat, flows, weight_col = "count")
#' clustered <- cluster_flows_dbscan(dmat, wvec, flows, eps = 8, minPts = 70)
#' @export
cluster_flows_dbscan <- function(dist_mat, w_vec, x, eps, minPts) {
  clustering <- dbscan::dbscan(
    x = dist_mat,
    eps = eps,
    minPts = minPts,
    weights = w_vec
  )
  cluster_df <- tibble::tibble(
    flow_ID = rownames(dist_mat),
    cluster = clustering$cluster
  )
  dplyr::inner_join(x, cluster_df, by = "flow_ID")
}


#' Sensitivity analysis of DBSCAN parameters for flow clustering. The function allows you to test 
#' different combinations of epsilon and minPts parameters for clustering flows using DBSCAN. It can be used 
#' to determine what parameter values make sense for your data
#'
#' @param dist_mat a precalculated distance matrix between desire lines (output of distance_matrix())
#' @param flows the original flows tibble (must contain flow_ID and 'count' column)
#' @param options_epsilon a vector of options for the epsilon parameter
#' @param options_minpts a vector of options for the minPts parameter
#' @param w_vec Optional precomputed weight vector (otherwise computed internally from 'count' column)
#'
#' @return a tibble with columns: id (to identify eps and minpts), cluster, size (number of desire lines in cluster), count_sum (total count per cluster)
#' @examples
#' flows <- sf::st_transform(flows_leeds, 3857)
#' flows = head(flows, 1000) # for testing
#' # Add flow lengths and coordinates
#' flows <- add_flow_length(flows)
#' # filter by length
#' flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
#' # Add x, y, u, v coordinates to flows
#' flows <- add_xyuv(flows)
#' # Calculate distance matrix
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' dmat <- distance_matrix(distances)
#' # Generate weight vector
#' w_vec <- weight_vector(dmat, flows, weight_col = "count")
#' 
#' Define the parameters for sensitivity analysis
#' options_epsilon <- seq(1, 10, by = 2)
#' options_minpts <- seq(10, 100, by = 10)
#' # # Run the sensitivity analysis
#' results <- dbscan_sensitivity(
#'    dist_mat = dmat,
#'    flows = flows,
#'    options_epsilon = options_epsilon,
#'    options_minpts = options_minpts,
#'    w_vec = w_vec
#'    )
#' @export
dbscan_sensitivity <- function(
    dist_mat,
    flows,
    options_epsilon,
    options_minpts,
    w_vec = NULL
) {
  options_parameters <- tidyr::expand_grid(eps = options_epsilon, minpts = options_minpts)
  results <- vector(mode = "list", length = nrow(options_parameters))
  if (is.null(w_vec)) {
    w_vec <- weight_vector(dist_mat, flows, weight_col = "count")
  }
  
  for (i in seq_len(nrow(options_parameters))) {
    message(
      sprintf(
        "running dbscan for option %d of %d : eps = %s | minpts = %s",
        i, nrow(options_parameters), options_parameters$eps[i], options_parameters$minpts[i]
      )
    )
    
    clustered_flows <- cluster_flows_dbscan(
      dist_mat = dist_mat,
      w_vec = w_vec,
      x = flows,
      eps = options_parameters$eps[i],
      minPts = options_parameters$minpts[i]
    )
    
    cluster_res <- clustered_flows |>
      dplyr::group_by(.data[["cluster"]]) |>
      dplyr::summarise(
        size = dplyr::n(),
        count_sum = sum(.data[["count"]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(id = sprintf("eps_%s_minpts_%s", options_parameters$eps[i], options_parameters$minpts[i])) |>
      dplyr::relocate(.data[["id"]], .data[["cluster"]], .data[["size"]], .data[["count_sum"]])
    
    
    results[[i]] <- cluster_res
  }
  dplyr::bind_rows(results)
}