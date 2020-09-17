## default parser
colparser <- setRefClass("colparser",
 fields = list(name="character"),
 methods = list(
   initialize = function(name="DEFAULT"){
     name <<- name
   },
   isvalid = function(val){
     return(val > 0)
   },
   filter = function(col){
     vectorizedValidation <- Vectorize(function(val){v <- isvalid(val)})
     c <- col[vectorizedValidation(col)]
   },
   parse = function(col, timecol, result, rownum, postfix) {
     # we dont want to be dividing 0, since validrows will always =< totalrows, its safe to set to 1 here
     totalrows <- max(1,length(col))
     # filter out invalid values
     c <- filter(col)
     validrows <- length(c)
     # find the min and max value in the vector
     range <- range(c,na.rm=TRUE)
     # write into dataframe
     #result[rownum,paste(name,"MIN", sep = "_", collapse = NULL)] <- range[1]
     #result[rownum,paste(name,"MAX", sep = "_", collapse = NULL)] <- range[2]
     #result[rownum,paste(name,postfix, "VALID ROWS", sep = "_", collapse = NULL)] <- length(c)
     result[rownum,paste(name,postfix, "COVERAGE", sep = "_", collapse = NULL)] <- paste(round(validrows/totalrows*100,digits=1),"%")
     result[rownum,paste(name,"MEDIAN", sep = "_", collapse = NULL)] <- median(c)
     result[rownum,paste(name,"LOW", sep = "_", collapse = NULL)] <- quantile(c, probs = 0.025)
     result[rownum,paste(name,"HIGH", sep = "_", collapse = NULL)] <- quantile(c, probs = 0.975)
     result
   },
   duration = function(col){
     starttime <- as_hms(as.character(first(col)))
     endtime  <- as_hms(as.character(last(col)))
     durationSeconds <- as.numeric( endtime - starttime )
   }
 )
)

#not used for now
spo2parser <- setRefClass("spo2parser",
  contains="colparser",
  methods = list(
    isvalid = function(val){
      return(val>0)
    }
  )
)

summaryparser <- setRefClass("summaryparser",
  contains="colparser",
  methods = list(
   parse = function(col, timecol, result, rownum, postfix) {
     # we dont want to be dividing 0, since validrows will always =< totalrows, its safe to set to 1 here
     totalrows <- max(1,length(col))
     # filter out invalid values
     c <- filter(col)
     validrows <- length(c)
     # find the min and max value in the vector
     range <- range(c,na.rm=TRUE)
     result[rownum,paste(name,postfix, "COVERAGE", sep = "_", collapse = NULL)] <- paste(round(validrows/totalrows*100,digits=1),"%")
     result
   }

 )
)
ST1parser <- setRefClass("ST1parser",
                          contains="colparser",
                          methods = list(
                            isvalid = function(val){
                              return(val>-32.7)
                            }
                          )
)

zeroAndAboveParser <- setRefClass("zeroAndAboveParser",
                         contains="colparser",
                         methods = list(
                           isvalid = function(val){
                             return(val>=0)
                           }
                         )
)


timeparser <- setRefClass("timeparser",
  contains="colparser",
  methods = list(
  parse = function(col, timecol, result, rownum, postfix, defaultRowDurationSeconds=10) {
    starttime <- as_hms(as.character(first(col)))
    endtime  <- as_hms(as.character(last(col)))
    #durationSeconds <- as.numeric( endtime - starttime )
    durationSeconds <- length(col) * defaultRowDurationSeconds
    result[rownum,"DURATION(min)"] <- round(durationSeconds/60)
    result[rownum,"Start Time"] <- starttime
    result[rownum,"End Time"] <- endtime
    result
    }
  )
)

## parser factory, maps header name to specific parsers
getparser <- function(name){
  parser <- switch(name,
    "Time"={timeparser$new(name)},
    "SpO2"={spo2parser$new(name)},
    "ST1"={ST1parser$new(name)},
    "FeAA"={zeroAndAboveParser$new(name)},
    "FiAA"={zeroAndAboveParser$new(name)},
    "FeN2O"={zeroAndAboveParser$new(name)},
    "FiN2O"={zeroAndAboveParser$new(name)},
    "FiCO2"={zeroAndAboveParser$new(name)},
    "MAC"={zeroAndAboveParser$new(name)})
  ## default parser
  if(is.null(parser)) parser <- colparser$new(name)
  parser
}

getsummaryparser <- function(name){
  parser <- switch(name,
                   "Time"={
                     timeparser$new(name)
                   })
  ## default parser
  if(is.null(parser)) parser <- summaryparser$new(name)
  parser
}
