#' Write All CSVs
#'
#' This function writes all dataframes in the global environment to CSVs.
#' @keywords write csv
#' @keywords write dataframe
#' @export
#' @examples
#' write.all.csvs()

write.all.csvs <- function(){
  
  data.frames <- ls(envir = .GlobalEnv)
  
  for(i in 1:length(data.frames)){
    
    if(is(get(data.frames[i]), "data.frame")){x<-i
      
      nm <- paste(data.frames[i],".csv",sep="")
      write.csv(data.frames[i],nm,row.names=F)
    
    }
  }
}

