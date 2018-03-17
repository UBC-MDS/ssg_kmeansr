context("Testing fit function")

# Generate test data frame
set.seed(46)
var <- .15
feature_one <- c(rnorm(5,-1, var),rnorm(5,0, var),rnorm(5,1, var))
feature_two <- c(rnorm(5,-1, var),rnorm(5,0, var),rnorm(5,1, var))

data_good <- data_frame(x1 = feature_one,
                        x2 = feature_two)

# Bad input - data frame with stings
data_bad_string<-data_good
data_bad_string[1,1]<-"susan"
data_bad_string[2,1]<-"GC"
data_bad_string[3,2] <-"sophia"

# Bad input - data frame with wrong dimentions
data_bad_1d<-data_good[,2]

# Bad input - empty data frame
data_bad_empty <- data.frame()

test_that("Check data integrity", {
  
  result <- fit(data_good, 3, "kmpp")
  
  # Expected outputs:
  expect_equal(typeof(result), "list")
  expect_equal(length(result), 3)
  expect_equal(is.data.frame(result[[1]]), TRUE)
  expect_equal(typeof(result[[2]]), "double")
  expect_equal(result[[2]]>0, TRUE)
  expect_equal(is.data.frame(result[[3]]), TRUE)
  
})

test_that('Error in input data', {
  # expected error:
  expect_error(fit(data_bad_string,3,"kmpp"), 'Input data must be numeric')
  expect_error(fit(data_bad_1d,3,"kmpp"), 'Input data must have 2 columns')
  expect_error(fit(data_bad_empty,3,"kmpp"), 'Input data cannot be empty')
})