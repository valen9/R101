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

#keep columns by name
data(iris)
iris

#Get names
grep("sepal",names(iris),ignore.case=T,value=T)
#Get column index
grep("sepal",names(iris),ignore.case=T,value=F)

iris$Length <- pmin(iris$Sepal.Length,iris$Petal.Length)
iris$Sepal <- pmax(iris$Sepal.Length,iris$Sepal.Width)


#caret ^ specifies that the leading string has to be empty
grep("Length",names(iris))
grep("^Length",names(iris))
#dollar $ specifies that the ending string has to be empty
grep("Sepal",names(iris))
grep("Sepal$",names(iris))

iris$var2 <- "a"

#with a digit, not ending with a digit, having a non-digit
grep("[[:digit:]]",names(iris))
grep("[^[:digit:]]$",names(iris))
grep("[^[:digit:]]",names(iris))
grep("[[:upper:]]",names(iris))
#... And so on. Check
#?regex



