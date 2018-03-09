# fit

context("Testing fit function")

# Load test data
data <- read_csv('../../data/sample_test.csv')

test_that("Check data integrity", {
  
  result <- fit(data, 3, "kmpp")
  
  # Expected outputs:
  expect_equal(typeof(result), "list")
  expect_equal(length(result), 3)
  expect_equal(is.data.frame(result[[1]]), TRUE)
  expect_equal(typeof(result[[2]]), "double")
  expect_equal(result[[2]]>0, TRUE)
  expect_equal(is.data.frame(result[[3]]), TRUE)
  
})