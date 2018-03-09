## kmeans++

#' Find the initial centroids using the kmeans++ approach
#' 
#' @param K The number of clusters
#' @return The indices of the centroids in the input data
#' @note This function is not optimized for performance or efficiency
kmpp <- function(data, K) {
  num_obs <- nrow(data)
  
  # sample the first centroid and add it to vector
  idx0 <- sample(1:num_obs, 1)
  centroids <- c()
  centroids <- c(centroids, idx0)  # as indices
  
  df <- tibble(
    x1 = unlist(data[,1]),
    x2 = unlist(data[,2])
  )
  
  # dist from a point to its nearest centroid
  get_dist <- Vectorize(function(x1, x2) {
    d <- c()  # distance vector
    # calc distance to all centroids in the vector
    for (cent in centroids) {
      tmp <- euc_dist(c(x1, x2), data[cent,])
      d <- c(d, tmp)
    }
    min(d)
  })
  
  # find which cluster a point belongs to
  get_clust <- Vectorize(function(x1, x2) {
    d <- c()
    for (cent in centroids) {
      tmp <- euc_dist(c(x1, x2), data[cent,])
      d <- c(d, tmp)
    }
    # use index as the cluster label
    which.min(d)
  })
  
  for (i in 1:K) {
    # the df gets updated every time a new centroid is calculated
    df <- df %>%
      mutate(
        D = get_dist(x1, x2),  # D as the min distance to the nearest centroid
        clust = get_clust(x1, x2)  # cluster of each data point
      ) %>%
      group_by(clust) %>%
      # not sure if I understand it correctly, it seems probability is based on
      # the distance within each cluster. So here I group the data by cluster first.
      mutate(
        prob = (D - min(D)) / (max(D) - min(D))
      ) %>%
      ungroup(clust)
    
    if (i < K) {
      # Get a new centroid based on probabilities and add it to the vector.
      # The functions, `get_dist` and `get_clust` will use the updated vector
      c_new <- sample(1:dim(data)[1], 1, prob=df$prob)
      centroids <- c(centroids, c_new)
    }
  }
  # print(centroids)
  # return(df)
  centroids
}