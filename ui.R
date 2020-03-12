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
      menuItem("Construct a Design", tabName = "constdes")#,
      # menuItem("Search for a Design", tabName = "searchdes")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .box { margin-bottom: 0; } 

      [class*="col-lg-"],[class*="col-md-"],
      [class*="col-sm-"],[class*="col-xs-"]{
        padding-right:0.00025 !important;
        padding-left:0.00025 !important;
      }
    '))),
    
    tabItems(
      tabItem("testdes", test_designUI(id = "testdes")), 
      tabItem("constdes", construct_designUI(id = "constdes"))#, 
      # tabItem("searchdes", search_designUI(id = "searchdes"))
    )
  )
  
)