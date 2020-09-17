library(hash)
datetimehash <- setRefClass("datetimehash",
  fields = list(map="hash",bucketsize="numeric"),
  methods = list(##default bucketsize=10 min
    initialize = function(bucketsize=600){
      map <<- hash()
      bucketsize <<- bucketsize
    },
    put = function(room,start,end,rowNum){
      fStart <- floor(start / bucketsize) * bucketsize
      fEnd <- floor(end / bucketsize) * bucketsize
      for(epoch in seq(fStart,fEnd,bucketsize)){
        key <- paste(room,epoch,sep=".")
        map[[key]] <<- c(rowNum,start,end)
      }
    },
    get = function(room,epoch){
      epoch <- floor(epoch/bucketsize)*bucketsize
      key <- paste(room,epoch,sep=".")
      val <- NULL
      if(has.key(key,map)){
        val <- map[[key]]
      }
      val
    },
    close = function(){
      clear(map)
    },
    getlength = function(){
      length(map)
    }
  )
)
