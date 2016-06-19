#' Keeps few levels from a factor in a dataframe
#' 
#' @param fac a string giving the factor's name
#' @param df a dataframe
#' @param fewkept an integer giving the number of levels to keep (excepting the "bulk")
#' @param control a string giving the control variable to sort by the levels
#' @return The vector of levels to keep
#' @examples 
#' fn_KeepFewLevels("speed",cars,control="dist")

fn_KeepFewLevels <- function(fac,df,fewkept=64,control=NA){
  library(data.table)
  temp <- data.table(df)
  
  if (!is.na(control)){
  tempfac <- temp[,by=list(fac=get(fac)),
                  list(Control=sum(get(control))
                  )]
    } else{ 
  tempfac <- temp[,by=list(fac=get(fac)),
                       list(Control=sum(!is.na(1:NROW(temp)))
                       )]
  }
  tempfac <- tempfac[order(-Control),]  
  
  return(as.character(head(tempfac[["fac"]],fewkept)))
  rm(temp);rm(tempfac);gc()
}
