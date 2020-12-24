# Function for module UI
search_designUI <- function(id) {
  ns <- NS(id)

  dashboardBody(
    tabItem(
      "searchdes",
      fluidRow(
        p("Hello World")
      ),
      fluidRow(
        box(
          width = 8, status = "info", solidHeader = TRUE,
          title = "Popularity by package (last 5 min)",
          p("Hello World")
        ),
        box(
          width = 4, status = "info",
          title = "Top packages (last 5 min)",
          # textOutput(ns(TO_Hello_user)),
          p("hello")
        )
      )
    )
  )
}

# Function for module server logic
search_design <- function(input, output, session) {

  # Show greetings
  output$TO_Hello_user <- renderText({
    return("Hello world !")
  })
}
