## Fit function for ssgkmeansr

library(tidyverse)

# Euclidean distance between any two points
euc_dist <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
}

# Find the centroid of a cluster
find_centroid <- function(dat) {
  mat <- as.matrix(dat)
  x1 <- mean(mat[,1])
  x2 <- mean(mat[,2])
  c(x1, x2)
}

# Check if converged
should_stop <- function(c0, c1, eps) {
  # c0 as the matrix of prev centroids
  # c1 as the matrix of the new centroids
  c_diff <- abs(c0 - c1)
  idx_diff <- c()
  idx_diff <- which(c_diff > eps)
  if (length(idx_diff) == 0) {
    # stop if converged
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# initalize centroids using random or kmpp
init_cent <- function(method, N, K, data) {
  m <- tolower(method)
  if (m %in% c('random', 'rand')) {
    centroids <- sample(1:N, K)
    return(centroids)
  } else if (m %in% c('kmpp', 'km++', 'kp')) {
    centroids <- kmpp(data, K)
    return(centroids)
  } else {
    warning('Invalid initialization method. Using default: random.')
    centroids <- sample(1:N, K)
    return(centroids)
  }
}

calcWitinSS <- function(data_cluster, centroid) {
  wss <- 0
  for (j in 1:nrow(data_cluster)) {
    wss <- wss + euc_dist(data_cluster[j,], centroid)
  }
  wss
}

#' A naive implementation of kmeans
#' 
#' @param data A data frame with attributes as columns and data points as rows
#' @param k Number of clusters
#' @param method Centroid initialization method. `random` or `kmpp`
#' @return List containing:
#'         1. Data frame of the attributes and clustering for each data point;
#'         2. Total within cluster sum of square;
#'         3. Data frame of k centroids
#' @export
fit <- function (raw, K, method='random') {
  data <- raw #input_preprocessing(raw, 'fit')
  nobs <- nrow(data)  # number of observations
  
  # initialize centroids as indices of data
  cent_init <- init_cent(method, nobs, K, data)
  centroids <- data[cent_init,]  # actual coords
  # distance matrix n by K: distance from each obs to each centroid
  dist_mat <- matrix(
    rep(0, nobs * K),
    nrow = nobs,
    ncol = K
  )
  # cluster labels
  labels <- rep(0, nobs)
  eps <- 0
  n_iter <- 0
  
  stop <- FALSE
  while (!stop) {
    # calc distance from each obs to each centroid
    for (row in 1:nobs) {
      for (idx_cent in 1:K) {
        dist_mat[row, idx_cent] <- euc_dist(data[row,], centroids[idx_cent,])
      }
      idx_min <- which.min(dist_mat[row,])
      # assign label based on the min distance
      labels[row] <- idx_min
    }
    
    # group data based on labels
    data_in_clust <- list()
    # new centroids based on labels
    centroids_new <- matrix(  # init
      rep(0, K*2),
      nrow = K,
      ncol = 2
    )
    for (k in 1:K) {
      indices <- which(labels == k)
      data_in_clust[[k]] <- data[indices,]
      centroids_new[k,] <- find_centroid(data_in_clust[[k]])
    }
    if (should_stop(centroids, centroids_new, eps)) {
      stop <- TRUE
    }
    # update centroids
    centroids <- centroids_new
    n_iter <- n_iter + 1
  }
  print(paste("kmeans converged in", n_iter, "runs."))
  
  data_with_labels <- tibble(x1 = data$x1,
                             x2 = data$x2,
                             cluster = labels)
  withinSS <- 0
  for (k in 1:K) {
    withinSS <- withinSS + calcWitinSS(data_in_clust[[k]], centroids[k,])
  }
  
  return(list(
    data = data_with_labels,
    withinSS = withinSS,
    centroids = centroids
  ))
}

# set.seed(1234)
raw <- read_csv('./data/sample_train.csv')
data <- raw %>% select(x1, x2)
K <- 3
data_fit <- fit(raw=data, K=K, method='kmpp')

data_fit$data %>%
  ggplot(aes(x1, x2)) +
    geom_point(aes(color = as.factor(cluster))) +
    scale_color_discrete('cluster')
