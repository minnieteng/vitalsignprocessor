library(hash)
test_that("put works", {
  d <- datetimehash$new(bucketsize=600)
  room <- "OR123"
  val <- 999991
  expected <- c(val,1234,2500)
  d$put(room,expected[2],expected[3],val)
  expect_equal(3,d$getlength())
  expect_equal(expected,d$get(room,1200))
  expect_equal(expected,d$get(room,1799))
  expect_equal(expected,d$get(room,1800))
  expect_equal(expected,d$get(room,2399))
  expect_equal(expected,d$get(room,2400))
  expect_equal(expected,d$get(room,2999))
})

test_that("put simulated range works", {
  d <- datetimehash$new(bucketsize=600)
  room <- "OR123"
  val <- 999991
  expected <- c(val,1420448374,1420459414)
  d$put(room,expected[2],expected[3],val)
  expect_equal(20,d$getlength())
  expect_equal(NULL,d$get(room,1420444680))
  expect_equal(expected,d$get(room,1420454640))
})
