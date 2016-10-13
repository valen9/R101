library(data.table)
library(gbm)
iris
iris.lm <- gbm(Sepal.Length ~ ., data=iris)
iris.lm

dt_iris <- data.table(iris)
dt_iris[,by=iris[,names(dt_iris)[-1]],COUNT:=.N]
MRP <- dt_iris[COUNT==max(COUNT)]

checky <- function(x,y)MRP[1,c(y):=x][1]
predcheck <- function(x,y)c(x,predict(iris.lm,newdata=checky(x,y),type="response",n.trees=100))
predcheck <- Vectorize(predcheck)

PDP_FC<-function(var2check){
SepWidth <- predcheck(unique(dt_iris[[var2check]]),var2check)
dt_iris$fitted <- predict(iris.lm,newdata=dt_iris,type="response",n.trees=100)
par(mar = c(5,5,2,5))
plot(table(dt_iris[[var2check]])/nrow(dt_iris)*100,type="h", ylab="Exposure",ylim=c(0,30))
par(new=T)
plot(x=SepWidth[1,],y=SepWidth[2,],col="green",type="p", axes=F,xlab=NA,ylab=NA,cex=1.2,pch=16)
axis(side=4)
mtext(side=4,line=3,'Prediction for MRP')
par(new=T)
plot(dt_iris[,by=get(var2check),mean(Sepal.Length)],axes=F,xlab=NA,ylab=NA,col="magenta",type="p")
par(new=T)
plot(dt_iris[,by=get(var2check),mean(fitted)],axes=F,xlab=NA,ylab=NA,col="darkgreen",type="p")
title(main=var2check)
}

PDP_FC("Sepal.Length")
PDP_FC("Sepal.Width")
PDP_FC("Petal.Length")
PDP_FC("Petal.Width")
PDP_FC("Species")
