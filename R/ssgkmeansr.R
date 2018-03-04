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

#' Predict k-means clustering; provide labels for all observations
#'
#' @param data A to-be-predicted new dataframe with attributes as columns and data points as rows. Data without cluster labels
#' @param  kmeans model from fit function; a data frame of centroids

#' @return List containing:1. data frame of the attributes；2.clustering label for each data point
#' @export
#'
#' @examples
#' kmpredict(test_data_frame, fit_model)


predict <- function() {
  # predict function
}

kmplot <- function() {
  # plot function
}