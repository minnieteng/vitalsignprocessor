library(dplyr)
library(futile.logger)
library(hash)
library("readxl")
library(vitalsignsprocessor)

#time bucket size, also the minimum time required to overlap for two durations to count as "overlap" or "same period"
thresholdSec <- 60 * 10
util <- datetimeutil$new()
source <- "testdata/aggregatedata.csv"
target <- "testdata/ORSOS_Data_1May2017.xlsx"
#CASE_NO	POST CASE ROOM	DATE	PROCEDURE_ID	DESCRIPTION	SERVICE	DR
#TIME ENTER ROOM	TIME EXIT ROOM	DIAGNOSIS	DECISION_TO_OPERATE_DATE	BIRTHDATE	CODE	DESCRIPTION	SHORT_NAME	PRIMARY_GROUP
targetdf <- read_excel(target,col_names = TRUE,.name_repair="universal",col_types=c("numeric","text","date","numeric","text","text","text",
                                                                                  "text","text","text","date","date","numeric","text","text","text")) %>%
  mutate(
    TIME.ENTER.ROOM=86400*as.numeric(TIME.ENTER.ROOM),
    TIME.EXIT.ROOM=86400*as.numeric(TIME.EXIT.ROOM),
    DATE=as.POSIXct(DATE,format="%Y-%m-%d %Z")
  )
df <- read.csv(source, header=TRUE, sep=",")
dtmap <- datetimehash$new(bucketsize=thresholdSec)

#create/fill the hashmap
for(i in 1:length(df$FILE)){
  rowinfo <- util$parserSourceRow(df[i,])
  dtmap$put(rowinfo$room,rowinfo$start,rowinfo$end,i)
}
print(names(targetdf))
mergedHeader <- c(names(df),names(targetdf))
resultdf <- data.frame(matrix(ncol=length(mergedHeader), dimnames=list(NULL, mergedHeader)))
resultRowNum <- 0
for(i in 1:length(targetdf$CASE_NO)){
  row <- targetdf[i,]
  room <- sub("#","",row$POST.CASE.ROOM)
  start <- as.numeric(row$DATE)+row$TIME.ENTER.ROOM;
  end <- as.numeric(row$DATE)+row$TIME.EXIT.ROOM;
  matchingRowNum <- util$getOverlappingRow(dtmap,room,start,end,thresholdSec)
  if(!is.null(matchingRowNum) && "ANESTHESIOLOGIST"==row$DESCRIPTION...14) {
    flog.info(paste("found match:",room,start,end,row$DESCRIPTION...14))
    resultRowNum <- resultRowNum+1
    for(i in 1:length(names(df))){
      resultdf[resultRowNum,i] <- as.character(df[matchingRowNum,i])
    }
    for(i in (length(names(df))+1):length(mergedHeader)){
      if(mergedHeader[i]=="TIME.ENTER.ROOM" || mergedHeader[i]=="TIME.EXIT.ROOM") {
        resultdf[resultRowNum,i] <- util$numericToTime(row$DATE,row[[i-length(names(df))]],"%H:%M:%S")
      } else resultdf[resultRowNum,i] <- as.character(row[[i-length(names(df))]])
    }
  }
}

if(resultRowNum>0){
  flog.info(paste("found",resultRowNum,"match(s)"))
  flog.info("write to output")
  write.csv(resultdf,"output.csv", row.names=TRUE)
} else {
  flog.info("no match found")
}
flog.info("clear cached hashmap")
dtmap$close()
flog.info("process completed")
