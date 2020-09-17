library(lubridate)
datafilter <- setRefClass("datafilter",
methods = list(
  findrange = function(df,header,threshold) {
    col <- df[,header]
    toprowindex <- min(which(col > threshold))
    if (toprowindex == Inf){
      return(NULL)
    }
    bottomrowindex <- max(which(col > threshold))
    range <- c(toprowindex, bottomrowindex)
  },
  durationByTimestampInSecs = function(col){
    starttime <- as_hms(as.character(first(col)))
    endtime  <- as_hms(as.character(last(col)))
    durationSeconds <- as.numeric( endtime - starttime )
  },
  duration = function(col, defaultColDurationSec){
    estimatedDurationSeconds <- defaultColDurationSec * length(col)
    result <- max(c(durationByTimestampInSecs(col),estimatedDurationSeconds))
    result
  },
  trim = function(df,header,threshold,durationSeconds, defaultColDurationSec=10){
    range <- findrange(df,header,threshold)
    if (is.null(range)){
      return(NULL)
    }
    df <- df[range[1]:range[2],]
    if(duration(df[,"Time"],defaultColDurationSec) > durationSeconds){
      result <- df
    } else {
      result <- NULL
    }
  },
  isvalidconsecutive = function(col, parser, startindex, threshold) {
    count <- 0
    for(i in seq(from=startindex, to=length(col), by=1)){
      if (!parser$isvalid(col[i]) || count>=threshold){
        break
      } else{
        count <- count + 1
      }
    }
    return(count>=threshold)
  },
  # threshold must be > 0
  findnextvalidindex = function(col, parser, startindex, threshold) {
    if(startindex < length(col)){
      for(i in seq(from=startindex, to=length(col), by=1)){
        if (parser$isvalid(col[i])){
          return(i)
        }
      }
    }
    return(length(col)+1)
  },
  getconsecutivecount = function(col, parser, startindex) {
    count <- 0
    if(startindex < length(col)){
      for(i in seq(from=startindex, to=length(col), by=1)){
        if (!parser$isvalid(col[i])){
          break
        } else{
          count <- count + 1
        }
      }
    }
    return(count)
  },
  # threshold must be > 0
  getallvalidconsecutives = function(col, parser, startindex, threshold){
    validconsecutives <- c()
    i <- startindex
    while(i<=length(col)){
      i <- findnextvalidindex(col,parser,i,threshold)
      countconsecutive <- as.numeric(getconsecutivecount(col,parser,i))
      if(countconsecutive>=threshold){
        range <- c(i,i+countconsecutive-1)
        validconsecutives <- c(validconsecutives,c(i,i+countconsecutive-1))
      }
      i <- i + countconsecutive
    }
    return(validconsecutives)
  },
  # threshold must be > 0
  # replace the original trim()
  trimwithvalidconsecutives = function(df,validatingcol,parser,threshold){
    allvalidconsecutives <- getallvalidconsecutives(validatingcol,parser,1,threshold)
    if(length(allvalidconsecutives)>=2){
      startindex <- as.numeric(allvalidconsecutives[1])
      endindex <- as.numeric(tail(allvalidconsecutives,n=1))
      df_trimmed <- df[startindex:endindex,]
    } else {
      df_trimmed <- NULL
    }
    return(df_trimmed)
  }
))
