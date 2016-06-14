#Let DF be the data frame you need to evaluate
install.packages("vcd")

DF <- as.data.frame(dplyr::nasa)
resp.var <- "year"
CRAMERsV<-function(x,dataIN) vcd::assocstats(xtabs(as.formula(paste("~",paste(c(x,resp.var),collapse="+"))),data=dataIN))[5]

CRAMER=data.frame(matrix(0,NROW(colnames(DF)),2))

colnames(CRAMER)<-c("Name","CRAMERsV")

for (i in 1:NROW(colnames(DF))){
  CRAMER[i,1]=colnames(DF)[i]
  if (colnames(DF)[i]!=resp.var) {CRAMER[i,2]=CRAMERsV(colnames(DF)[i],DF)}else{CRAMER[i,2]=1 }
}
CRAMER<-CRAMER[order(-CRAMER[,2]),]

