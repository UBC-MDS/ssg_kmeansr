# test functions for ssgkmeansr

context("Testing kmpredict function")

test_that("Predicting results match expectations", {
	
  # load test data
  
  test_data <- read_csv('../../data/sample_test.csv')
  n <-n_row(test_data)
  
  output <- kmpredict(test_data,3,"random")
  
  # expected outputs:
  expect_equal(length(output[-1]),n ) 
  expect_equal(output[-1]<4, TRUE)
  expect_equal(is.data.frame(output[1]), TRUE)
  expect_equal(typeof(output[2]), "double")
  expect_equal(is.data.frame(output[3]), TRUE)
  
})