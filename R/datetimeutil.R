datetimeutil <- setRefClass("datetimeutil",methods = list(
  initialize = function(){
  },
  getDateFromFilePath = function(date,time,format){
    datetime <- paste(date,time)
    epoch <- as.numeric(as.POSIXct(datetime,tz="UTC",format=format))
  },
  numericToTime = function(dateStr,numericTime,format){
    #dateStr: yyyy-mm-dd
    dt <- as.POSIXct(numericTime, origin=dateStr)
    f <- format(dt,format=format,tz="UTC")
  },
  #thresholdSec: how much time does these two time needs to overlap to count as match
  isOverlappingPeriod = function(start1,end1,start2,end2,thresholdSec){#epoch in seconds
    isOverlappingPeriod <- min(c(end1,end2)) - max(c(start1,start2)) > thresholdSec
  },
  parserSourceRow = function(row){
    filename <- strsplit(as.character(row$FILE),"\\\\|[^[:print:]]",fixed=FALSE)
    filenameseg <- strsplit(filename[[1]][3],"_")
    room <- filenameseg[[1]][1]
    date <- filenameseg[[1]][2]
    start <- getDateFromFilePath(date,row$Start.Time,"%Y%m%d %H:%M:%S");
    end <- getDateFromFilePath(date,row$End.Time,"%Y%m%d %H:%M:%S");
    duration <- row$DURATION.min. * 60
    end <- max(c(end,start+duration))
    result <- list(start=start,end=end,room=room)
  },
  getOverlappingRow = function(hashmap,room,start,end,thresholdSec){
    d <- hashmap$get(room,start)
    if(is.null(d)){
      d <- hashmap$get(room,end)
    }
    if(!is.null(d) && isOverlappingPeriod(d[2],d[3],start,end,thresholdSec)){
      return(d[1])
    }
    return(NULL)
  }
))
