#write coverage summary into csv
library(dplyr)
library(hms)
library(futile.logger)
library(vitalsignprocessor)

log_name="logfile.log"
result_file_name="coverageresult.csv"
headers <- c("HR","ST1","ST2","ST3","Imped.","P1sys","P1dia","P1mean","PR.P1.","P2sys","P2dia","P2mean","PR.P2.","P3sys","P3dia","P3mean","PR.P3.","P4sys","P4dia","P4mean","PR.P4.","NIBPsys","NIBPdia","NIBPmean","PR.NIBP.","T1","T2","T3","T4","SpO2","PR.SpO2.","SpO2_ir","SVO2p","EtCO2","FiCO2","RR.CO2.","Pamb","FeO2","FiO2","FeN2O","FiN2O","FeAA","FiAA","MAC","RR.Spiro.","Ppeak","PEEP","Pplat","TVinsp","TVexp","Compl","MVexp","C.O.","Tblood","RVEF","PCWP","T1.","TOF.","PTC","HR.ECG.","HRmax","HRmin","SvO2","P5sys", "P5dia","P5mean","PR.P5.","P6sys","P6dia", "P6mean","PR.P6.")
filterheader <- "SpO2" ##the variable we use to 1. trim dataframe, and 2. filter files
rowthreshold <- 10 ## number of consecutive rows from the top and bottom
timethreshold <- 900 ## duration(seconds) between first and last valid filterheader column
stdin <- "list.txt"
files <- readLines(stdin)
filecount <- 1
result <- data.frame()
for (file in files){
  appender.tee <- flog.info(paste("start processing file: ",file))
  flog.appender(appender.tee(log_name))
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
  for(header in headers){
    parser <- getsummaryparser(header)
    result <- parser$parse(df_trimmed[,header],df_trimmed[,"Time"],result,filecount,"trimmed")
  }
  filecount <- filecount + 1
}
flog.info(paste("files processing complete: ",filecount-1, " files processed"))
flog.info(paste("writing result into file:",file,result_file_name,sep=" "))
write.csv(result,result_file_name)


