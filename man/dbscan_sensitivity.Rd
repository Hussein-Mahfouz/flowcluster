% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{dbscan_sensitivity}
\alias{dbscan_sensitivity}
\title{Sensitivity analysis of DBSCAN parameters for flow clustering. The function allows you to test
different combinations of epsilon and minPts parameters for clustering flows using DBSCAN. It can be used
to determine what parameter values make sense for your data}
\usage{
dbscan_sensitivity(
  dist_mat,
  flows,
  options_epsilon,
  options_minpts,
  w_vec = NULL
)
}
\arguments{
\item{dist_mat}{a precalculated distance matrix between desire lines (output of distance_matrix())}

\item{flows}{the original flows tibble (must contain flow_ID and 'count' column)}

\item{options_epsilon}{a vector of options for the epsilon parameter}

\item{options_minpts}{a vector of options for the minPts parameter}

\item{w_vec}{Optional precomputed weight vector (otherwise computed internally from 'count' column)}
}
\value{
a tibble with columns: id (to identify eps and minpts), cluster, size (number of desire lines in cluster), count_sum (total count per cluster)
}
\description{
Sensitivity analysis of DBSCAN parameters for flow clustering. The function allows you to test
different combinations of epsilon and minPts parameters for clustering flows using DBSCAN. It can be used
to determine what parameter values make sense for your data
}
\examples{
flows <- sf::st_transform(flows_leeds, 3857)
flows <- head(flows, 1000) # for testing
# Add flow lengths and coordinates
flows <- add_flow_length(flows)
# filter by length
flows <- filter_by_length(flows, length_min = 5000, length_max = 12000)
# Add x, y, u, v coordinates to flows
flows <- add_xyuv(flows)
# Calculate distance matrix
distances <- flow_distance(flows, alpha = 1.5, beta = 0.5)
dmat <- distance_matrix(distances)
# Generate weight vector
w_vec <- weight_vector(dmat, flows, weight_col = "count")

# Define the parameters for sensitivity analysis
options_epsilon <- seq(1, 10, by = 2)
options_minpts <- seq(10, 100, by = 10)
# # Run the sensitivity analysis
results <- dbscan_sensitivity(
  dist_mat = dmat,
  flows = flows,
  options_epsilon = options_epsilon,
  options_minpts = options_minpts,
  w_vec = w_vec
)
}
