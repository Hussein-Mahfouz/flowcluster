#' Calculate Flow Distance and Dissimilarity
#' @param x tibble with flow_ID, x, y, u, v, length_m
#' @param alpha numeric, origin weight
#' @param beta numeric, destination weight
#' @return tibble of all OD pairs with fd, fds columns
#' @examples
#' distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
#' @export
flow_distance <- function(x, alpha = 1, beta = 1) {
  # check that alpha and beta are pstive number, and that they add up to 2
  if (!is.numeric(alpha) || !is.numeric(beta) || alpha <= 0 || beta <= 0) {
    stop("Alpha and beta must be positive numbers.")
  }
  if (alpha + beta != 2) {
    stop("Alpha and beta must sum to 2.")
  }
  grid <- tidyr::expand_grid(flow_ID_a = x$flow_ID, flow_ID_b = x$flow_ID)
  grid <- dplyr::inner_join(grid, dplyr::select(x, flow_ID, x, y, u, v, length_m),
                            by = c("flow_ID_a" = "flow_ID")) |>
    dplyr::rename_with(~paste0(.x, "_i"), c("x", "y", "u", "v", "length_m")) |>
    dplyr::inner_join(dplyr::select(x, flow_ID, x, y, u, v, length_m),
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
#' dmat <- distance_matrix(distances)
#' @export
distance_matrix <- function(distances, distance_col = "fds") {
  distances |>
    dplyr::select(flow_ID_a, flow_ID_b, tidyselect::all_of(distance_col)) |>
    tidyr::pivot_wider(names_from = flow_ID_b, values_from = tidyselect::all_of(distance_col)) |>
    tibble::column_to_rownames(var = "flow_ID_a")
}

#' Generate Weight Vector from Flows
#' @param dist_mat distance matrix
#' @param x flows tibble with flow_ID and weight_col
#' @param weight_col column to use as weights (default = "count")
#' @return numeric weight vector
#' @examples
#' wvec <- weight_vector(dmat, flows, weight_col = "count")
#' @export
weight_vector <- function(dist_mat, x, weight_col = "count") {
  x |>
    dplyr::select(flow_ID, tidyselect::all_of(weight_col)) |>
    dplyr::inner_join(
      tibble::tibble(flow_ID = rownames(dist_mat)),
      by = "flow_ID"
    ) |>
    dplyr::pull(tidyselect::all_of(weight_col))
}

#' Cluster Flows using DBSCAN
#' @param dist_mat distance matrix
#' @param w_vec weight vector
#' @param x flows tibble with flow_ID
#' @param eps DBSCAN epsilon parameter
#' @param minPts DBSCAN minPts parameter
#' @return flows tibble with cluster column
#' @examples
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