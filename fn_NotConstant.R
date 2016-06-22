fn_NotConstant <- function(x){which(sapply(x,function(x)length(unique(x))!=1))}
