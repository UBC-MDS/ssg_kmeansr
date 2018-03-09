# test functions for ssgkmeansr

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