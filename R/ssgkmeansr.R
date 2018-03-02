#' Compute k-means clustering
#' 
#' Build clusters and save cluster attributes with random selection or k-means ++  centroid initialization
#'
#' @param data A data frame with attributes as columns and data points as rows
#' @param k Number of clusters
#' @param method Centroid initialization method. `random` or `kmpp``
#'
#' @return List containing: 1. data frame of the attributes and clustering for each data point; 2.total within cluster sum of square; 3. data frame of k centroids
#' @export
#'
#' @examples
#' fit(my_data_frame,3,"kmpp")

fit <- function(data, k, method) {
}

predict <- function() {
  # predict function
}

kmplot <- function() {
  # plot function
}