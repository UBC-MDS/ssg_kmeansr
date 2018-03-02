# ssgkmeansr
library(tidyverse)

fit <- function() {
  # fit fucntion for kmeans
}

predict <- function() {
  # predict function
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
