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

headers <- c("HR","NIBPsys","NIBPdia","NIBPmean","PR.P1.","T1","SpO2","PR.SpO2.","EtCO2","RR.CO2.","FeO2","FiO2","FeN2O","FiN2O","FeAA","FiAA","MAC","Ppeak","PEEP","TVinsp","TVexp","MVexp","P1sys","P1dia","P1mean","Compl")
plotter <- ggplotter$new()
summarydf <- read.csv("summarydata.csv", header=TRUE, sep=",")
metadata <- data.frame()
for (header in headers){
  summaryheadername <- plotter$getsummaryname(header)
  valist <- as.numeric(summarydf[,gsub(" ",".",summaryheadername)])
  metadata[header,"median"] <- median(valist)
  metadata[header,"2.5percentile"] <- quantile(valist, probs = 0.025)
  metadata[header,"97.5percentile"] <- quantile(valist, probs = 0.975)
}
#print(metadata)
write.csv(metadata,"metadata.csv",row.names=TRUE)
