
###########################################
# ui.r
# 
# MoMo data mock-up visualizations in R.
# This is the user interface file for RShiny.
# 
# Adrian Albert
# Last modified: June 2013.
###########################################

library(shiny)
library(shinyIncubator)
options(RCHART_LIB = 'polycharts')

# this should come from the server side
monitors = read.csv('~/Dropbox/WellDone/WellDone-Data-Simulator-Dashboard/data/monitors.csv')
sensor_list = as.list(monitors$ID)
names(sensor_list) = monitors$Name

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("MoMo Dashboard"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  
  sidebarPanel(
    
    # select sensor location
    selectInput("ID", "Sensor Location:", sensor_list),    
    br(), 
    
    # prepare Slidify/Sweave LaTex pdf report
    downloadButton('prepareReportBtn', 'Prepare Report'),
    br(),
    
    # plot map
    div(class="span16", style="margin-top:50px", 
        plotOutput("map.alerts")),
    br()    
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  
  mainPanel(
    tabsetPanel(
      tabPanel("General View", plotOutput("general")), 
      tabPanel("Sensor Locations", plotOutput("map.tot")), 
      tabPanel("Weather Data", plotOutput("weather.series")), 
      tabPanel("Sensor View", showOutput("sensor", "polycharts")), 
      tabPanel("Summary Statistics", tableOutput("summary")),
      tabPanel("Raw data", tableOutput("raw"))
    )
  )
))
