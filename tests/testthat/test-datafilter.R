library(dplyr)
library(hms)
test_that("findrange(): SPO2 all valid", {
  df <- data.frame("SPO2"=c(120, 1, 2))
  filter <- datafilter$new()
  r <- filter$findrange(df, "SPO2", 0)
  expect_equal(r,c(1,3))
})

test_that("findrange(): SPO2 last is invalid", {
  df <- data.frame("SPO2"=c(120, 1, 2, 0))
  filter <- datafilter$new()
  r <- filter$findrange(df, "SPO2", 0)
  expect_equal(r,c(1,3))
})

test_that("findrange(): SPO2 first is invalid", {
  df <- data.frame("SPO2"=c(0, 1, 2))
  filter <- datafilter$new()
  r <- filter$findrange(df, "SPO2", 0)
  expect_equal(r,c(2,3))
})

test_that("findrange(): SPO2 all invalid", {
  df <- data.frame("SPO2"=c(0, 0, 0))
  filter <- datafilter$new()
  r <- filter$findrange(df, "SPO2", 0)
  expect_equal(r,NULL)
})

test_that("durationByTimestampInSecs(): valid seconds", {
  v <- c("6:38:10","6:38:20","6:38:30")
  filter <- datafilter$new()
  r <- filter$durationByTimestampInSecs(v)
  expect_equal(r,20)
})

test_that("durationByTimestampInSecs():valid mins", {
  v <- c("6:38:10","6:38:20","6:58:30")
  filter <- datafilter$new()
  r <- filter$durationByTimestampInSecs(v)
  expect_equal(r,1220)
})

test_that("valid durationByTimestampInSecs hours", {
  v <- c("6:18:10","6:38:20","7:58:30")
  filter <- datafilter$new()
  r <- filter$durationByTimestampInSecs(v)
  expect_equal(r,6020)
})

test_that("negative durationByTimestampInSecs", {
  v <- c("6:18:10","6:38:20","6:18:09")
  filter <- datafilter$new()
  r <- filter$durationByTimestampInSecs(v)
  expect_equal(r,-1)
})

test_that("durationByTimestampInSecs(): negative duration", {
  v <- c("6:18:10","6:38:20","6:18:09")
  filter <- datafilter$new()
  r <- filter$durationByTimestampInSecs(v)
  expect_equal(r,-1)
})

test_that("trim(): valid duration 15 mintes and 1 sec", {
  df <- data.frame("SPO2"=c(120, 1, 2),
                   "Time"=c("6:18:10","6:38:20","6:33:11"))
  filter <- datafilter$new()
  r <- filter$trim(df,"SPO2",0,900)
  expect_equal(r,df)
})

test_that("trim(): valid duration 15 min", {
  df <- data.frame("SPO2"=c(120, 1, 2),
                   "Time"=c("6:18:10","6:38:20","6:33:10"))
  filter <- datafilter$new()
  r <- filter$trim(df,"SPO2",0,900)
  expect_equal(r,NULL)
})

test_that("isvalidconsecutive(): SpO2 valid", {
  col <- c(120, 1, 2)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$isvalidconsecutive(col,parser,1,3)
  expect_equal(r,TRUE)
})

test_that("isvalidconsecutive(): SpO2 invalid one short", {
  col <- c(120, 1, 2)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$isvalidconsecutive(col,parser,2,3)
  expect_equal(r,FALSE)
})

test_that("isvalidconsecutive(): SpO2 all invalid", {
  col <- c(0, 0, 0)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$isvalidconsecutive(col,parser,1,3)
  expect_equal(r,FALSE)
})

test_that("isvalidconsecutive(): SpO2 one invalid", {
  col <- c(1, 1, 0)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$isvalidconsecutive(col,parser,1,3)
  expect_equal(r,FALSE)
})
test_that("readFirstLineFromFile(): works", {
  f <- "../testdata/OR1_20160107_1130.txt"
  n <- gutil$new()
  r <- n$readFirstLineFromFile(f)
  expect_equal(r,c("01/07/2016 11:30:05 AM","Trend","1452195005"))
})
test_that("getallvalidconsecutives(): works", {
  col <- c(0,0,1,2,3,0,4,5,6,7,8,0,0,1,2,3)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$getallvalidconsecutives(col,parser,1,3)
  expect_equal(c(3,5,7,11,14,16),r)
})

test_that("getallvalidconsecutives(): works with first range invalid", {
  col <- c(0,0,1,2,0,0,4,5,6,7,8,0,0,1,2,3)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$getallvalidconsecutives(col,parser,1,3)
  expect_equal(c(7,11,14,16),r)
})

test_that("getallvalidconsecutives(): works with last range invalid", {
  col <- c(0,0,1,2,3,0,4,5,6,7,8,0,0,1,2)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$getallvalidconsecutives(col,parser,1,3)
  expect_equal(c(3,5,7,11),r)
})

test_that("getallvalidconsecutives(): works with no valid range", {
  col <- c(0,0,1,2,0,0,4,5,0,0,0,0,0,1,2)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$getallvalidconsecutives(col,parser,1,3)
  expect_equal(c(),r)
})

test_that("getallvalidconsecutives(): works with no valid range", {
  col <- c(0,0,1,2,3,0,4,5,0,0,0,0,3,1,2)
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  r <- filter$getallvalidconsecutives(col,parser,1,3)
  expect_equal(c(3,5,13,15),r)
})

test_that("trimwithvalidconsecutives(): works with valid range", {
  df <- data.frame("SPO2"=c(1, 2, 3),"Time"=c("1:00","2:00","3:00"))
  col <- df[,"SPO2"]
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  df_trimmed <- filter$trimwithvalidconsecutives(df,col,parser,3)
  expect_equal(df,df_trimmed)
})

test_that("trimwithvalidconsecutives(): works with no valid range", {
  df <- data.frame("SPO2"=c(-1,-1,-1),"Time"=c("1:00","2:00","3:00"))
  col <- df[,"SPO2"]
  parser <- getparser("SpO2")
  filter <- datafilter$new()
  df_trimmed <- filter$trimwithvalidconsecutives(df,col,parser,3)
  expect_equal(NULL,df_trimmed)
})

