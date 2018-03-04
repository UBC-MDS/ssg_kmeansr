# test functions for ssgkmeansr

context("Testing predict function")

test_that("Predicting results match expectations", {
	
  # load test data
  
  test_data <- read_csv('../../data/sample_test.csv')
  n <-n_row(test_data)
  
  output <- predict(test_data,list(c(1,2), c(2,3)))
  
  # expected outputs:
  expect_equal(length(output[,ncol(test_data)],n ) 
  expect_equal(max(output[,ncol(test_data)])<4, TRUE)
  expect_equal(is.data.frame(output), TRUE)
  
})