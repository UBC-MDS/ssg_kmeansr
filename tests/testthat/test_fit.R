# fit

context("Testing fit function")

test_that("Check data integrity", {
  # Load test data
  data <- read_csv('../../data/sample_test.csv')
  result <- fit(data,3,"random")
  
  # Expected outputs:
  expect_equal(typeof(result), "list")
  expect_equal(length(result), 3)
  expect_equal(is.data.frame(result[1]), TRUE)
  expect_equal(typeof(result[2]), "double")
  expect_equal(result[2]>0, TRUE)
  expect_equal(is.data.frame(result[3]), TRUE)
  
})