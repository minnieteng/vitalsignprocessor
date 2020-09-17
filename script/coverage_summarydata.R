#get metadata

library(dplyr)
library(hms)
library(ggplot2)
library(easyGgplot2)
library(lattice)
library(futile.logger)
library(LaplacesDemon)
library(ggpubr)
library(xlsx)

operationrooms <- c("OR1","OR2","OR3","OR4","OR5","OR6","OR7","DENTAL","CT","CATH")
#headers <- c("HR","ST1")
headers <- c("HR","NIBPsys","NIBPdia","NIBPmean","PR.P1.","T1","SpO2","PR.SpO2.","EtCO2","RR.CO2.","FeO2","FiO2","FeN2O","FiN2O","FeAA","FiAA","MAC","Ppeak","PEEP","TVinsp","TVexp","MVexp","P1sys","P1dia","P1mean","Compl")
FILE_POSTFIX <- "_coverageresult.csv"
plist <- list()
summarylist <- list()
for (operationroom in operationrooms){
  file <- paste(operationroom,FILE_POSTFIX,sep="")
  df <- read.csv(file, header=TRUE, sep=",")
  for (header in headers){
    coverage <- as.numeric(sub("%", "", df[,paste(header,"_trimmed_COVERAGE",sep = "")], fixed = TRUE))
    summarytitle <- paste(header,"summary")
    summarylist[[summarytitle]] <- c(summarylist[[summarytitle]],coverage)
    }
  }

summarydata <- data.frame(summarylist)


#print(head(summarylist))

write.csv(summarydata,"summarydata.csv",row.names=FALSE)


