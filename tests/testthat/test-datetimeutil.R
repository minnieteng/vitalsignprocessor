test_that("getDateFromFilePath works", {
  d <- datetimeutil$new()
  epoch <- d$getDateFromFilePath("20150105","08:59:34","%Y%m%d %H:%M:%S")
  expect_equal(1420448374,epoch)
})


test_that("isOverlappingPeriod with overlapping", {
  d <- datetimeutil$new()
  isOverlap <- d$isOverlappingPeriod(1420448374,1420459414,1420444500,1420453620,600)
  expect_equal(TRUE,isOverlap)
})


test_that("isOverlappingPeriod with no overlapping", {
  d <- datetimeutil$new()
  isOverlap <- d$isOverlappingPeriod(1420448374,1420459414,1421444500,1421453620,600)
  expect_equal(FALSE,isOverlap)
})
