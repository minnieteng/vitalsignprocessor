#get aggregate graphs
##graphing script
library(dplyr)
library(hms)
library(ggplot2)
library(easyGgplot2)
library(lattice)
library(futile.logger)
library(LaplacesDemon)
library(ggpubr)

pdf(file = "aggregatePlots.pdf",height = 100, width = 20)
theme_set(theme_classic()+theme(legend.position="top"))
#operationrooms <- c("OR1","OR2","OR3","OR4","OR5","OR6","OR7","DENTAL","CT","CATH")
operationrooms <- c("OR1")
headers <- c("HR","ST1","ST2","ST3","Imped.","P1sys","P1dia","P1mean","PR.P1.","P2sys","P2dia","P2mean","PR.P2.","P3sys","P3dia","P3mean","PR.P3.","P4sys","P4dia","P4mean","PR.P4.","NIBPsys","NIBPdia","NIBPmean","PR.NIBP.","T1","T2","T3","T4","SpO2","PR.SpO2.","SpO2_ir","SVO2p","EtCO2","FiCO2","RR.CO2.","Pamb","FeO2","FiO2","FeN2O","FiN2O","FeAA","FiAA","MAC","RR.Spiro.","Ppeak","PEEP","Pplat","TVinsp","TVexp","Compl","MVexp","C.O.","Tblood","RVEF","PCWP","T1.","TOF.","PTC","HR.ECG.","HRmax","HRmin","SvO2","P5sys", "P5dia","P5mean","PR.P5.","P6sys","P6dia", "P6mean","PR.P6.")
#headers <- c("HR")
FILE_POSTFIX <- "_aggregatedata.csv"
plotter <- aggregateplotter$new(colpostfixes=c("_HIGH","_MEDIAN","_LOW"))
plist <- plotter$getplots(operationrooms, headers)
result_plots <- multiplot(plotlist=plist,cols = length(operationrooms))
print(result_plots)
dev.off()
flog.info("graph generation completed")

