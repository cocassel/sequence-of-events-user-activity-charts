#
# This is the user-interface definition of a Shiny web application. 
#
# Make sure the Shiny package is pre-installed on your machine
#
#


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  #Application title
  titlePanel("Macaron Usage Data Visualization Dashboard"),
  
  #sidebarpanel for filters
  sidebarLayout(
    sidebarPanel(
      # Button for reset date    
      actionButton( inputId = "reset", label = "Reset Date Filter"),
  #Date filter
          dateRangeInput(inputId = 'dateRange1',
                     label = 'Date Filter',
                     min = "2015-06-01" ,
                     max = Sys.Date() + 5 ,
                     start = "2015-06-01",
                     end = Sys.Date() + 5
      ),
    # Sequence Chart Filter   
      selectInput("select_id", label = "Select Session Id for Sequence Chart", 
                  choices = list("Loading..." = 1), 
                  selected = 1)
    ),
    # Plot all charts in the mainpanel
    mainPanel(plotOutput("sessionOverTime"),
              plotOutput("actionPerSession"),
              plotOutput("sessionQuartileLine"),
              plotOutput("actionPerSecond"),
              plotOutput("actionfrequency"),
              plotOutput("sessionDuration"),              
              plotOutput("sessionDurationTypeH"),
              plotOutput("SequencePlot")),position = c("right")
  )
 )
)
