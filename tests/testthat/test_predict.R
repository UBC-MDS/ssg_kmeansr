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


context("Testing predict function")

test_that("Predicting results match expectations", {
	
  # load test data
  
  test_data <- read_csv('../../data/sample_test.csv')
  n <-nrow(test_data)
  centroids <- as.tibble(rbind(c(0,0),c(1,1),c(-1,-1)))
                         
  output <- predict(test_data, centroids)
  
  # expected outputs:
  expect_equal(nrow(output[,ncol(test_data)]), n) 
  expect_equal(max(output[,ncol(test_data)])<4, TRUE)
  expect_equal(is.data.frame(output), TRUE)
  
})