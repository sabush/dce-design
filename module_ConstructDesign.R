# Function for module UI
construct_designUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      width = 12,
      h3("Design Dimensions"),
      numericInput(ns("num_fac"), "Number of factors", 2, min = 2,
                   max = 16),
      numericInput(ns("num_opt"), "Number of options per choice Set", 2, min = 2,
                   max = 8)
    ),
    fluidRow(
      width = 12,
      actionButton(inputId = ns("enter_levels"), label = "Enter Levels"),
      actionButton(inputId = ns("reset"), label = "Reset"),
      h1(textOutput(ns("enter_design")), style = "font-size:0.1px"),
      h1(textOutput(ns("display_output")), style = "font-size:0.1px")
    ),
    
    # Enter choice sets
    # conditionalPanel('output.enter_design == "TRUE"', ns = ns, 
    fluidRow(
      width = 12,
      h3("Design Details"),
      column(
        width = 4,
        h4("Enter the number of levels for each factor:"),
        rHandsontableOutput(ns("hot"))
      ),
      column(
        width = 8,
        h4("Enter the generators used to construct choice sets here:"),
        rHandsontableOutput(ns("hot_gens")), 
        p(HTML("Each row is a single set of generators. Levels are labelled (0,1,...,<i>l</i>), where <i>l</i> is the number of levels for each factor.")),
        p("OptA_FacB corresponds to the level of factor B in the generator for option A of the choice set. The generator for Option 1 is (0,0,...,0)."),
        p("Right click on a cell of the table to add or remove a generator.")
      )
    ),
    fluidRow(
      width = 12,
      actionButton(inputId = ns("test_design"), label = "Test Design"),
      br()
      # )
    ),
    # Display Output
    conditionalPanel('output.display_output == "TRUE"', ns = ns, 
                     width = 12, 
                     column(width = 12,
                            h3("Design Assessment:"),
                            p('The choice sets for this design is:'),
                            uiOutput(ns('csets')),
                            p('The information matrix for this design is:'),
                            uiOutput(ns('FImatrix')),
                            htmlOutput(ns("designInfo"))
                     )
                     
    )
  )
  
}

# Function for module server logic
construct_design <- function(input, output, session) {

  
  values = reactiveValues(
    reset_val = 0,
    display_output = 'FALSE',
    enter_design = 'FALSE'
  )
  
  output$enter_design <- renderText({invisible(values$enter_design)})
  output$display_output <- renderText({invisible(values$display_output)})
  observeEvent(input$enter_levels, {values$enter_design <- 'TRUE'})
  observeEvent(input$test_design, {values$display_output <- 'TRUE'})
  observeEvent(input$reset, {values$enter_design <- 'FALSE'})
  observeEvent(input$reset, {values$display_output <- 'FALSE'})
  
  myDF <- reactive({
    DF <- NULL
    if (!is.null(input$hot)){ #If hot changes, 
      DF <- hot_to_r(input$hot) #then update DF and also update DF in values,
      values[['DF']] <- DF
    } else if(!is.null(isolate(values$DF))){ #otherwise use the DF in values.
      DF <- isolate(values$DF)
    } else {
      req(input$num_fac)
      DF <- data.frame(NumLevels = as.integer(rep(2,as.numeric(input$num_fac))),
                       row.names = paste('Fac', 1:as.numeric(input$num_fac))) %>% 
        dplyr::select(NumLevels)
      values[['DF']] <- DF
      values$reset_val <- 1
    }
    DF
  }) %>% debounce(1000) #debounce slows down so that reactive expression gets less "chatty"
  
  assessDesign <- eventReactive(input$test_design, {
    validate(need(!anyNA(values$gens),"The set of generators contains NA values"),
             need(!anyNA(values$DF[,1]),"The set of levels contains NA values"))
    generators <- as.matrix(values$gens)
    levels <- values$DF[,1]
    assess_design(levels, generators = generators, print_detail = F)
  })
  
  myDF_gens <- reactive({
    gens <- NULL
    
    if (!is.null(input$hot_gens)){ #If hot changes, 
      gens <- hot_to_r(input$hot_gens) #then update DF and also update DF in values,
      values[['gens']] <- gens
    } else if(!is.null(isolate(values$gens))){ #otherwise use the DF in values.
      gens <- isolate(values$gens)
    } else {
      req(input$num_fac)
      gens <- data.frame(entry = as.integer(rep(NA,as.numeric(input$num_fac) * (as.numeric(input$num_opt) - 1) * 1)),
                          entry_opt = paste0('Opt', rep(2:as.numeric(input$num_opt), each = as.numeric(input$num_fac), times = 1)),
                          entry_fac = paste0('Fac', rep(1:as.numeric(input$num_fac), times = (as.numeric(input$num_opt) - 1) * 1)),
                          chset = as.integer(rep(1:1, each = as.numeric(input$num_fac) * (as.numeric(input$num_opt) - 1))), 
                          stringsAsFactors = FALSE) %>% 
        unite("entry_label", entry_opt:entry_fac) %>% 
        pivot_wider(names_from = entry_label, values_from = entry) %>% 
        dplyr::select(-chset)     
      values[['gens']] <- gens
      values$reset_val <- 1
    }
    gens
  }) %>% debounce(1000) #debounce slows down so that reactive expression gets less "chatty"
  
  output$hot = renderRHandsontable({
    if (isolate(values$reset_val)){
      DF = values$DF
    } else DF = myDF()
    if (!is.null(DF)) rhandsontable(DF)
  })
  
  output$hot_gens = renderRHandsontable({
    if (isolate(values$reset_val)){
      gens = values$gens
    } else gens = myDF_gens()
    if (!is.null(gens)) rhandsontable(gens)
  })
  
  
  observeEvent(input$enter_levels, {
    req(input$num_fac)
    req(input$num_opt)
    values$DF <- data.frame(NumLevels = as.integer(rep(2,as.numeric(input$num_fac))),
                            row.names = paste('Fac', 1:as.numeric(input$num_fac)))
    
    values$gens <- data.frame(entry = as.integer(rep(NA,as.numeric(input$num_fac) * (as.numeric(input$num_opt) - 1) * 1)),
                               entry_opt = paste0('Opt', rep(2:as.numeric(input$num_opt), each = as.numeric(input$num_fac), times = 1)),
                               entry_fac = paste0('Fac', rep(1:as.numeric(input$num_fac), times = (as.numeric(input$num_opt) - 1) * 1)),
                               chset = as.integer(rep(1:1, each = as.numeric(input$num_fac) * (as.numeric(input$num_opt) - 1))), 
                               stringsAsFactors = FALSE) %>% 
      unite("entry_label", entry_opt:entry_fac) %>% 
      pivot_wider(names_from = entry_label, values_from = entry) %>% 
      dplyr::select(-chset)
    
    values$reset_val <- 1
  })  
  
  
  output$csets <- renderTable({
    ad <- assessDesign()
    ad$choicesets
  }, colnames = F, digits = 0)
  
  output$FImatrix <- renderTable({
    ad <- assessDesign()
    ad$cmat
  }, colnames = F)
  
  # Show greetings
  output$designInfo <- renderText({
    ad <- assessDesign()
    HTML(paste0("The determinant of the C matrix is: <b>",signif(ad$detmatc, 4), 
                "</b><br/>The efficiency compared with complete factorial (optimal): <b>",signif(ad$efficiency,4), '%<b/>')) 
  })
  
  
  
}