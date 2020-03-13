library(shiny)
library(shinydashboard)

# load module functions
source("module_TestDesign.R")

ui <- dashboardPage(skin = "purple",
                    title = "Discrete Choice Designer",
  dashboardHeader(title = "Discrete Choice Designer", 
                  titleWidth = 250
                  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Test a Design", tabName = "testdes"),
      menuItem("Construct a Design", tabName = "constdes"),
      # menuItem("Search for a Design", tabName = "searchdes")
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      p('Created by Stephen Bush (2020)'),
      p('based on Mathematica code by'),
      p('Deborah Street and Leonie Burgess')
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .box { margin-bottom: 0.01; } 

      [class*="col-lg-"],[class*="col-md-"],
      [class*="col-sm-"],[class*="col-xs-"]{
        padding-right: 2px !important;
        padding-left: 2px !important;
      }
      
      .container-fluid {
        padding-right: 0px;
        padding-left: 0px;
        margin-right: auto;
        margin-left: auto;
      }
    '))),
    
    tabItems(
      tabItem("testdes", test_designUI(id = "testdes")), 
      tabItem("constdes", construct_designUI(id = "constdes"))#, 
      # tabItem("searchdes", search_designUI(id = "searchdes"))
    )
  )
  
)