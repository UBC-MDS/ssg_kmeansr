context("Testing kmplot function")

# Generate test data frame
set.seed(46)
var <- .15
feature_one <- c(rnorm(5,-1, var),rnorm(5,0, var),rnorm(5,1, var))
feature_two <- c(rnorm(5,-1, var),rnorm(5,0, var),rnorm(5,1, var))
cluster<-c(rep(1,5), rep(2,5), rep(3,5))

data_good <- data_frame(x1 = feature_one,
                        x2 = feature_two,
                        cluster = cluster)

# Bad input - data frame with stings
data_bad_string<-data_good
data_bad_string[1,1]<-"susan"
data_bad_string[2,1]<-"GC"
data_bad_string[3,2] <-"sophia"

# Bad input - data frame with wrong dimentions
data_bad_2d<-data_good[, c(2,3)]

# Bad input - empty data frame
data_bad_empty <- data.frame()

test_that("Plot mapping match expectations", {
  # load test data
  p <- kmplot(data_good)
  
  # expected outputs:
  expect_equal(toString(p$mapping$x), 'x1')
  expect_equal(toString(p$mapping$y), 'x2')
  expect_equal(toString(p$layers[[1]]$mapping$colour), 'cluster')
})

test_that('Check data integrity', {
  p <- kmplot(data_good)
  # expected outputs:
  # check if any data is dropped in the plot
  expect_equal(length(p$data$cluster), nrow(data_good))
})

test_that('Error in input data', {
  # expected error:
  expect_error(kmplot(data_bad_string), 'Input data must be numeric')
  expect_error(kmplot(data_bad_2d), 'Input data must have 3 columns')
  expect_error(kmplot(data_bad_empty), 'Input data cannot be empty')
})