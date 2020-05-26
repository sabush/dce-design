server <- function(input, output, session) {
  callModule(module = test_design, id = "testdes")
  callModule(module = construct_design, id = "constdes")
  callModule(module = find_design, id = "finddes")
}
