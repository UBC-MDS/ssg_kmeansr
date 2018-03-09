# test functions for ssgkmeansr

library(tidyverse)
# Generate test data frame
# Read in correct data frame
data_good <- read.csv('../../data/sample_test.csv')

# Bad input - data frame with stings
data_bad_string<-data_good
data_bad_string[1,1]<-"susan"
data_bad_string[2,1]<-"GC"
data_bad_string[3,2] <-"sophia"

# Bad input - data frame with wrong dimentions
data_bad_2d<-data_good[, c(2,3)]

# Bad input - empty data frame
data_bad_empty <- data.frame()


context("Testing predict function")

test_that("Predicting results match expectations", {
	
  # load test data
  
  n <-nrow(data_good)
  centroids <- as.tibble(rbind(c(0,0),c(1,1),c(-1,-1)))
                         
  output <- predict(data_good, centroids)
  
  # expected outputs:
  expect_equal(nrow(output[,ncol(test_data)]), n) 
  expect_equal(max(output[,ncol(test_data)])<4, TRUE)
  expect_equal(is.data.frame(output), TRUE)
  
})

test_that('Error in input data', {
  # expected error:
  expect_error(kmplot(data_bad_string), 'Input data must be numeric')
  expect_error(kmplot(data_bad_2d), 'Input data must have 3 columns')
  expect_error(kmplot(data_bad_empty), 'Input data cannot be empty')
})