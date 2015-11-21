library(shiny)
library(datasets)
library(pROC)

# We tweak the "am" field to have nicer factor labels. Since
# this doesn't rely on any user inputs we can do this once at
# startup and then use the value throughout the lifetime of the
# application

DATA<-data.frame(HairEyeColor)
DATAEXP=DATA[rep(1:nrow(DATA),DATA$Freq),]

# Define server logic required to plot various variables against
# mpg
shinyServer(function(input, output) {
  
  # Compute the formula text in a reactive expression since it is
  # shared by the output$caption and output$mpgPlot functions
  formulaText <- reactive({
    
    DATAEXP$RESPONSE=as.numeric(DATAEXP$Eye==input$eyecolor)
    fit<-glm(DATAEXP$RESPONSE~.,
             family=binomial(link="logit"),
             data=DATAEXP[,c("Hair","Sex")])
    PRED=predict(fit,newdata=DATAEXP,type="response")
    roccurve=roc(DATAEXP$RESPONSE,PRED)
    plot(roccurve)
    plot(smooth(roccurve),add=T,col="blue")
    
  })
  
  # Return the formula text for printing as a caption

  
  # Generate a plot of the requested variable against mpg and
  # only include outliers if requested
  output$ROCPLOT <- renderPlot({
   formulaText()
    })
  
  output$HISTO <- renderPlot({
    
    DATAEXP$RESPONSE=as.numeric(DATAEXP$Eye==input$eyecolor)
    hist(DATAEXP$RESPONSE,freq=F)
    
  })
})

