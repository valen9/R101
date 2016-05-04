#Hack List

data(cars)
cars
cars[25,1]=NA

#count levels

cntl <- function(x)length(unique(x))

cntl(cars)
cntl(cars$speed)
sapply(cars,cntl)

#count missing

cntm <- function(x)sum(is.na(x))

cntm(cars)
cntm(cars$speed)
sapply(cars,cntm)

#detect key

iskey <- function(x){length(unique(x))==length(x)}

iskey(cars)
iskey(cars$speed)
sapply(cars,iskey)



