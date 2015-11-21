library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HairEyeColor database"),
  
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("eyecolor", "Variable:",
                  c("Blue"="Blue",
                    "Green"="Green",
                    "Brown"="Brown",
                    "Hazel"="Hazel"
                    ))
    ),
    
    # Show the caption and plot of the requested variable against
    # mpg
    mainPanel(
      plotOutput("ROCPLOT"),
      plotOutput("HISTO")
    )
  )
))
