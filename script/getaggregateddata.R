library(dplyr)
library(hms)
library(futile.logger)
library(stringr)
library(lubridate)
library(vitalsignsprocessor)
result_file_name="aggregatedata.csv"
headers <- c("Time","HR","NIBPsys","NIBPdia","NIBPmean","PR.P1.","T1","SpO2","PR.SpO2.","EtCO2","RR.CO2.","FeO2","FiO2","FeN2O","FiN2O","FeAA","FiAA","MAC","Ppeak","PEEP","TVinsp","TVexp","MVexp","P1sys","P1dia","P1mean","Compl")
filterheader <- "SpO2" ##the variable we use to 1. trim dataframe, and 2. filter files
rowthreshold <- 10 ## number of consecutive rows from the top and bottom
timethreshold <- 900 ## duration(seconds) between first and last valid filterheader column
util <- gutil$new()
stdin <- "list.txt"
files <- readLines(stdin)
filecount <- 1
result <- data.frame()
for (file in files){
  flog.info(paste("start processing file: ",file))
  df <- read.csv(file, header=TRUE, sep="\t", skip=1)
  filter <- datafilter$new()
  parser <- getparser(filterheader)
  df_trimmed <- filter$trimwithvalidconsecutives(df, df[,filterheader], parser, rowthreshold)
  if (is.null(df_trimmed)){
    flog.info(paste("skipping file:",file,"because no data after trimming"))
    next
  }
  result[filecount,"FILE"] <- file
  filename <- strsplit(file, "_")[[1]][1]
  result[filecount,"OR"] <- gsub("[A-z \\.\\(\\):]", "", filename)
  fileinfo <- util$readFirstLineFromFile(file)
  rawdate <- strsplit(fileinfo[[1]][1], "\\s+")[[1]][1]
  date <- str_replace_all(rawdate,"/","-")
  print(date)
  result[filecount,"Day"] <- wday(as.Date(date,'%m-%d-%Y'),label=TRUE)
  for(header in headers){
    parser <- getparser(header)
    result <- parser$parse(df_trimmed[,header],df_trimmed[,"Time"],result,filecount,"trimmed")
  }
  filecount <- filecount + 1
}
flog.info(paste("files processing complete: ",filecount-1, " files processed"))
flog.info(paste("writing result into file:",result_file_name))
write.csv(result,result_file_name)


