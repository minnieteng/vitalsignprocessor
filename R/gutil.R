#graph library
gutil <- setRefClass("gutil",
 methods = list(
   getbin = function(binsize,vec){
     bin <- vector()
     for(val in vec){
       if(val==0) bin <- c(bin,val)
       else bin <- c(bin,floor(val/binsize)*binsize)
     }
     df <- data.frame(density=vec,bin=bin)
   },
   readFirstLineFromFile = function(filepath) {
      con = file(filepath, "r")
      line = readLines(con, n = 1)
      close(con)
      linelist <- strsplit(line, '\t')
      lineitem <- unlist(linelist)
   }
 )
)
