
fn_KeepFewLevels <- function(fac,df,fewkept=64,control){
temp <- data.table(df)
tempfac <- temp[,by=list(fac=get(fac)),
                list(Control=sum(get(control))
            )]
tempfac <- tempfac[order(-Control),]  

return(as.character(head(tempfac[["fac"]],fewkept)))  
rm(temp);rm(tempfac);gc()
}
