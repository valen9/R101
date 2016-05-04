#data handling comparison
library(dplyr)
library(data.table)

rm(list=ls(all=T))
gc()

df <- data.frame(gp = factor(rep(letters[1:3], each = 100000)),
                 y = rnorm(300000))

#base R
base_gp <- function() {
agg_mean <- aggregate(df$y, by = list(df$gp), mean)
agg_std <- aggregate(df$y, by = list(df$gp), sd)
base_gp <- data.frame(gp = agg_mean$Group.1, mean = agg_mean$x, sd = agg_std$x)
}

system.time(ds <- base_gp())

#dplyr
dplyr_gp <- function(){
  dplyr_gp <- summarise(group_by(df, gp), mean=mean(y), sd=sd(y))
}

system.time(ds <- dplyr_gp())


#data.table
dt_gp <- function(){
dt <-  data.table(df)
ds <- dt[,c(mean(y),sd(y)),by=gp]
}

system.time(ds <- dt_gp())
