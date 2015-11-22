#' Make a Chicken Dinner
#'
#' This function allows you to make a perfect chicken dinner.
#' @param weight The weight of your chicken in kilograms. No default.
#' @param end.time Character string. The time you want your dinner to be ready in the format "hh:mm". If not set will asume asap.
#' @param smart TRUE or FALSE. When set to TRUE will return an error if the requested time set in end.time is not possible. Defaults to TRUE.
#' @keywords chicken
#' @keywords time
#' @export
#' @examples
#' Chicken.Time(1.35, "18:55", TRUE)

Chicken.Time <- function(weight, end.time, smart=TRUE){
  
  chicken.mins <- (25*(weight/0.5))+25
  chicken.rest <- 15
  heat.oven <- 20
  total.time <- heat.oven + chicken.mins + chicken.rest
  
  if(missing(end.time)){
    start.time <- Sys.time()
    end.time <- start.time + (total.time*60)
    possible <- TRUE
  }else{
    temp <- as.character(Sys.time())
    temp <- paste(substr(temp,1,10),end.time)
    end.time <- as.POSIXlt(temp)
    start.time <- end.time - (total.time*60)
    
    possible <- ifelse((Sys.time()-start.time)>0,FALSE,TRUE)
   
  }
  
  
  chicken.in <- substr(as.character(end.time - ((chicken.mins + chicken.rest)*60)),12,16)
  potatoes.boil <- substr(as.character(end.time - (60*60)),12,16)
  potatoes.in <- substr(as.character(end.time - (60*40)),12,16)
  carrots.in <- substr(as.character(end.time - (60*40)),12,16)
  stuffing.in <- substr(as.character(end.time - (60*25)),12,16)
  chicken.out <- substr(as.character(end.time - (chicken.rest*60)),12,16)
  yorkshires <- substr(as.character(end.time - (60*5)),12,16)
  gravy <- substr(as.character(end.time - (60*5)),12,16)
  start.time <- substr(as.character(start.time),12,16)
  end.time <- substr(as.character(end.time),12,16)
  
  actions <- data.frame(c("Put oven on 180C","Put chicken in",
                  "Potatoes on to boil",
                  "Potatoes in oven","Carrots in oven",
                  "Start stuffing","Take chicken out to rest",
                  "Put Yorkshires in","Make gravy","Done"))
  
  times<- data.frame(c(start.time,chicken.in,potatoes.boil,potatoes.in,carrots.in,
                         stuffing.in,chicken.out,yorkshires,gravy,end.time))
  
  output<- cbind(actions,times)
  
  names(output) <- c("Action","Time")
  
  if(smart == TRUE){
    
    if(possible == FALSE){
      return("Not enough Time")}
  
  }else{
    View(output)
    write.csv(output, "chicken.csv", row.names=F)}

}
