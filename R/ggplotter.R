library(dplyr)
library(hms)
library(ggplot2)
library(lattice)
library(futile.logger)
library(LaplacesDemon)
library(ggpubr)
ggplotter <- setRefClass("ggplotter",
 methods = list(
   #generate a violin plot
   violinplot = function(data,title){
     flog.info(paste("plotting",title,"..."))
     plot <- ggplot2.violinplot(data=data,mapping=aes(x=value),yName=frequency,
                                geom_boxplot(width = 0.2),orientation="horizontal",
                                mainTitle=title,
                                trim=FALSE) + ylim(0, 100)
   },
   #generate plot for all operationroom-variables
   getplotlist = function(finalist){
     plist <- list()
     for(title in names(finalist)){
       plist[[title]] <- violinplot(finalist[[title]],title)
     }
     plist
   },
   #get list of coverage values (%) from dataframe
   getplotdata = function(df, header){
     data <- as.numeric(sub("%", "", df[,paste(header,"_trimmed_COVERAGE",sep = "")], fixed = TRUE))
   },
   getsummaryname = function(header){
     name <- paste(header,"summary")
   },
   #main function to call
   getplots = function(operationrooms, headers){
     summarylist <- list()
     datalist <- list()
     for (operationroom in operationrooms){
       file <- paste("data/",operationroom,FILE_POSTFIX,sep="")
       df <- read.csv(file, header=TRUE, sep=",")
       for(header in headers){
         title <- paste(operationroom,header)
         datalist[[title]] <- getplotdata(df, header)
         summarylist[[getsummaryname(header)]] <- c(summarylist[[getsummaryname(header)]],datalist[[title]])
       }
     }
     plots <- getplotlist(c(datalist,summarylist))
   },
   getdefinitions = function(variables){ #temporary placeholder
     result <- c()
     for(variable in variables){
       result <- c(result,paste(variable," definition"))
     }
     result
   },
   getmedians = function(variables){ #temporary placeholder
     result <- c()
     for(variable in variables){
       result <- c(result,0)
     }
     result
   },
   getcoverages = function(variables,plots){
     result <- c()
     for(variable in variables){
       result <- c(result,plots[[getsummaryname(variable)]])
     }
     result
   },
   getmetadata = function(plots,rownames){
     colnames <- c("Definition","Coverage_plot","Median")
     data <- c(getdefinitions(rownames),getcoverages(rownames,plots),getmedians(rownames))
     result <- array(data,dim=c(length(rownames),length(colnames)),dimnames=list(rownames,colnames))
   }
 ))

aggregateplotter <- setRefClass("aggregateplotter",
   contains="ggplotter",
   fields=list(colpostfixes="vector"),
   methods = list(
    #get list of values from dataframe
      getplotdata = function(df, header){
         tryCatch({
            data <- as.numeric(df[,header])
         },error=function(cond){
            message(paste("unable to find column:",header))
            message(cond)
            return(c())
         })

      },
      violinplot = function(data,title){
         flog.info(paste("plotting",title,"..."))
         plot <- ggplot2.violinplot(data=data,mapping=aes(x=value),yName=frequency,
                                    geom_boxplot(width = 0.2),orientation="horizontal",
                                    mainTitle=title,
                                    trim=FALSE)
      },
      getplots = function(operationrooms, headers){
         datalist <- list()
         for (operationroom in operationrooms){
            file <- paste(operationroom,FILE_POSTFIX,sep="")
            print(file)
            df <- read.csv(file, header=TRUE, sep=",")
            for(header in headers){
               for(colpostfix in colpostfixes){
                  title <- paste(operationroom,header,sub("_","",colpostfix,fixed=TRUE))
                  datalist[[title]] <- getplotdata(df, paste(header,colpostfix,sep=""))
               }
            }
         }
         plots <- getplotlist(datalist)
      }
   )
)


