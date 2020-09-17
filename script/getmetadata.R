library(dplyr)
library(hms)
library(futile.logger)
library(vitalsignsprocessor)
result_file_name="metatestresult.csv"
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
  df_trimmed <- filter$trim(df, filterheader, 0, timethreshold) # threshold > 0 ##900 seconds = 15min
  parser <- getparser(filterheader)
  if (is.null(df_trimmed) ||
      !filter$isvalidconsecutive(df_trimmed[,filterheader],parser,1,rowthreshold) ||
      !filter$isvalidconsecutive(df_trimmed[,filterheader],parser,length(df_trimmed[,filterheader])-rowthreshold,rowthreshold)) {
    flog.info(paste("skipping file:",file," because it does not contain valid rows"))
    next
  }
  result[filecount,"FILE"] <- file
  fileinfo <- util$readFirstLineFromFile(file)
  result[filecount,"Datestamp"] <- fileinfo[1]
  for(header in headers){
    #print(header)
    parser <- getparser(header)
    #result[filecount,"TOTAL ROWS"] <- nrow(df)
    #result[filecount,"TOTAL TRIMMED ROWS"] <- nrow(df_trimmed)
    #result <- parser$parse(df[,header],df[,"Time"],result,filecount,"untrimmed")
    result <- parser$parse(df_trimmed[,header],df_trimmed[,"Time"],result,filecount,"trimmed")
  }
  filecount <- filecount + 1
}
#tparser <- timeparser$new()
#result <- tparser$parse(df_trimmed[,header],result,filecount)
flog.info(paste("files processing complete: ",filecount-1, " files processed"))
flog.info(paste("writing result into file:",result_file_name))
write.csv(result,result_file_name)


