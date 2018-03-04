# test functions for ssgkmeansr

context("Testing kmplot function")

test_that("Plot mapping match expectations", {
  # load test data
  data <- read_csv('../../data/sample_train.csv')
  p <- kmplot(data)
  # expected outputs:
  expect_equal(toString(p$mapping$x), 'x1')
  expect_equal(toString(p$mapping$y), 'x2')
  expect_equal(toString(p$layers[[1]]$mapping$colour), 'cluster')
})

test_that('Check data integrity', {
  data <- read_csv('../../data/sample_train.csv')
  p <- kmplot(data)
  
  # check if any data is dropped in the plot
  expect_equal(length(p$data$cluster), nrow(data))
})

test_that('Error in input data', {
  expect_error(kmplot(), 'Error in input data')
})