#' Cluster origin-destination pairs (flows) with dbscan
#' 
#' @param x A matrix of origin-destination pairs (flows) with two columns (TBC)
#' @param eps The maximum distance between two samples for one to be considered as in the neighborhood of the other. This is not a maximum bound on the distances of points within a cluster. This is the most important DBSCAN parameter to choose appropriately for your data set and distance function.
#' @param min_pts The number of samples in a neighborhood for a point to be considered as a core point. This includes the point itself.
#' @param dist_method The distance method to use. See \code{\link{dist}} for details.
#' 
#' @return A vector with group assignments for each flow (TBC)
#' @export
#' @examples
#' \dontrun{
#' flowcluster(flowdata, eps = 0.1, min_pts = 5, dist_method = "euclidean")
#' }
flowcluster = function(x, eps, min_pts, dist_method = "euclidean") {
    message("hello world")
}