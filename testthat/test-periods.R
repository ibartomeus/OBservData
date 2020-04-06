library(testthat)
library(dplyr)
context("checks that period values are valid")

base_data <- read.csv('../data/rodent_abundance.csv', 
                      stringsAsFactors = F)

test_that("period numbers are valid", {
  
  expect_true(all(base_data$period < 1000))
  
})
