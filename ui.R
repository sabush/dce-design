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
    tabItems(
      tabItem("testdes", test_designUI(id = "testdes")), 
      tabItem("constdes", construct_designUI(id = "constdes"))#, 
      # tabItem("searchdes", search_designUI(id = "searchdes"))
    )
  )
  
)