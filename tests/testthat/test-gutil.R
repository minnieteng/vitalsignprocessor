#test gutil function#
library(dplyr)
library(hms)
test_that("readFirstLineFromFile(): works", {
  n <- gutil$new()
  r <- n$readFirstLineFromFile("../testdata/OR1_20160107_1130.txt")
  expect_equal(r[1],"01/07/2016 11:30:05 AM")
  expect_equal(r[2],"Trend")
  expect_equal(r[3],"1452195005")
})
