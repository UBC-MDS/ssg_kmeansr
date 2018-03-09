## predict function
# library(tidyverse)

predict <- function(data, centroids) {
  nobs <- nrow(data)  # num of observations
  K <- nrow(centroids)  # number of clusters
  
  # init for calculation
  dist_mat <- matrix(
    rep(0, nobs * K),
    nrow = nobs,
    ncol = K
  )
  labels <- rep(0, nobs)
  
  for (row in 1:nobs) {
    for (idx_cent in 1:K) {
      dist_mat[row, idx_cent] <- euc_dist(data[row,], centroids[idx_cent,])
    }
    idx_min <- which.min(dist_mat[row,])
    # assign label based on the min distance
    labels[row] <- idx_min
  }
  
  res <- data %>%
    mutate(cluster = labels)
  res
}

data_test <- read_csv('./data/sample_test.csv')
res <- predict(data_test, data_fit$centroids)

res %>%
  ggplot(aes(x1, x2)) +
  geom_point(aes(color = as.factor(cluster))) +
  scale_color_discrete('cluster')