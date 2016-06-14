# Load some data for illustration
install.packages("dplyr")

NASA <- as.data.frame(dplyr::nasa) 
getwd()
# Saved it as a CSV
write.csv(NASA,"nasa.csv")

# Start of illustration
  INPUT="nasa.csv"
  # Read the header
  COLN=scan(INPUT,what="",sep=",",nlines=1)
  # Define a function to count the rows
  count=function(x)sum(!is.na(x))

  # Pick the variable to read from COLN
  SEL=rep("NULL",length(COLN));SEL[which(COLN=="ozone")]="character"
  
  # Read only the selected variable
  DATA=read.csv(INPUT,colClasses=SEL)

  # Analyze
  FREQ=aggregate(DATA[,1],by=list(DATA[,1]),count)
  plot(FREQ)

 


