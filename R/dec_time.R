
#' Calculate time in hh:mm format from decimal time 
#' 
#' 
#' @description calculates time in 23:30 from 23.5
#' 
#' 
#' @param dectime time vector in decimal numeric format
#' @return character time vector in hh:mm format
#' @author Maite Lezama Valdes
#' @examples
#' dt <- c(23.1, 23.5, 23.6)
#' dec_time(dt)
#' 

dec_time <- function(dectime){
  h <- sapply(seq(length(dectime)), function(i){
    if(dectime[i] < 1){ # if <1h
      paste0("00:",dectime[i]*60)
    } else if((dectime[i] - floor(dectime[i]))==0){ # if no minutes
      paste0(dectime[i], ":00")
    } else {
      if(floor(dectime[i])<10){
        paste0("0", floor(dectime[i]), ":", round((dectime[i]-floor(dectime[i]))*60, digits=0))
      } else if(round((dectime[i]-floor(dectime[i]))*60, digits=0) <10 ){
        paste0(floor(dectime[i]), ":", "0", round((dectime[i]-floor(dectime[i]))*60, digits=0))
      } else{
        paste0(floor(dectime[i]), ":", round((dectime[i]-floor(dectime[i]))*60, digits=0))
      }
    }
  })
  return(h)
}
