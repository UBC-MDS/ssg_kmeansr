<<<<<<< HEAD
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

=======
# ssgkmeansr
library(tidyverse)

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
>>>>>>> upstream/master
fit <- function(data, k, method) {
}

#' Predict k-means clustering; provide labels for all observations
#'
#' @param data A to-be-predicted new dataframe with attributes as columns and data points as rows. Data without cluster labels
#' @param  kmeans model from fit function; a data frame of centroids

#' @return dataframe containing new data and clustering lable for each data point
#' @export
#'
#' @examples
#' predict(test_data, list(c(1,2), c(2,3)))


predict <- function(data,vector) {
  
}


#' Visualize kmeans results
#'
#' @param dat dataframe. Data with cluster labels
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' data <- tibble(x1=c(1,2,10), x2=c(1,3,10), cluster=c(1,1,2))
#' fig <- kmplot(data)
kmplot <- function(dat) {
  tryCatch({
    # to-do: rename cols to, e.g. x1, x2, ...
    data <- dat %>%
      mutate(cluster = as.factor(cluster))
    
    fig <- data %>%
      ggplot(aes(x1, x2)) +
        geom_point(aes(color = cluster)) +
        scale_color_discrete('cluster') +
        theme_bw()
    
    return(fig)
  }, error = function(e) {
    stop(e)
  })
}
