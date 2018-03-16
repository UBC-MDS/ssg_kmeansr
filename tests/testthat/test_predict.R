context("Testing predict function")

# Generate test data frame
# Read in correct data frame
data_good <- read.csv('../../data/sample_test.csv')

# Bad input - data frame with stings
data_bad_string<-data_good
data_bad_string[1,1]<-"susan"
data_bad_string[2,1]<-"GC"
data_bad_string[3,2] <-"sophia"

# Bad input - data frame with wrong dimentions
data_bad_1d<-data_good[,2]

# Bad input - empty data frame
data_bad_empty <- data.frame()

test_that("Predicting results match expectations", {
  
  n <-nrow(data_good)
  centroids <- as.tibble(rbind(c(0,0),c(1,1),c(-1,-1)))

  output <- predict(data_good, centroids)
  
  # expected outputs:
  expect_equal(nrow(output[,ncol(data_good)]), n) 
  expect_equal(max(output[,ncol(data_good)])<4, TRUE)
  expect_equal(is.data.frame(output), TRUE)
  
})

test_that('Error in input data', {
  # expected error:
  expect_error(predict(data_bad_string), 'Input data must be numeric')
  expect_error(predict(data_bad_1d), 'Input data must have 2 columns')
  expect_error(predict(data_bad_empty), 'Input data cannot be empty')
})