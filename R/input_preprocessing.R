## Input preprocessing
library(tidyverse)


#' Convert raw input data to data frame
#'
#' @param raw Raw input
#' @param stage Fit or predict
#'
#' @return Cleaned data in tibble format
#' @export
#'
#' @examples
input_preprocessing <- function(raw, stage) {
  tryCatch({
    data <- as_tibble(raw)
  }, error = function(e) {
    stop('Failed to convert data to data frame.')
  })
  
  ncols <- 0
  if (stage == 'fit') {
    ncols <- 2
  } else {
    ncols <- 3
  }
  
  if (ncol(data) != ncols) {
    stop(paste('Input data must have', ncols, 'columns.'))
  }
  if (nrow(data) <= 0) {
    stop('Input data cannot be empty.')
  }
  if (!is.numeric(unlist(data[,1]))) {
    stop('Input data must be numeric')
  }
  if (!is.numeric(unlist(data[,2]))) {
    stop('Input data must be numeric')
  }
  
  # change column names
  if (stage == 'fit') {
    colnames(data) <- c('x1', 'x2')
  } else if (stage == 'predict') {
    colnames(data) <- c('x1', 'x2', 'cluster')
  }
  
  # change cluster column to factor type
  if (stage != 'fit' & ncol(data) == 3) {
    data <- data %>% mutate(
      cluster = as.factor(cluster)
    )
  }
  
  return(data)
}