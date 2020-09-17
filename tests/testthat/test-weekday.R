#test weekday feature#
library(dplyr)
library(hms)

test_that("initialize works", {
  testfilepath <- "../testdata/OR1_20160107_1130.txt"

  con = file(testfilepath)
  line = readLines(con, n = 1)
  vline = array(line)
  #print(vline)
  close(con)
  linelist <- strsplit(vline, '\t')
  lineitem <- unlist(linelist)

  #print(lineitem)
  date <- strsplit(lineitem[[1]][1], "\\s+")[[1]][1]
  #print(date)

  day <- weekdays(as.Date(date))
  #print(day)
})
