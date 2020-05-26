# Function for module UI
find_designUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Enter the number of levels for each factor, the number of options for each
    # choice set and the effects to be estimated
    box(width = 4,title = "Design Dimensions", status = "primary", solidHeader = T,
        strong("Enter the number of levels for each factor:"),
        p("One factor per row, right click on table to add or remove rows."),
        rHandsontableOutput(ns("hot")),
        br(),
        numericInput(ns("num_opt"), "Number of options per choice set", 2, min = 2,
                     max = 8),
        numericInput(ns("num_runs"), "Number of choice sets in the experiment", 4, min = 2,
                     max = 100),
        numericInput(ns("num_iter"), "Number of iterations of the exchange algorithm", 100, min = 1,
                     max = 1000),
        radioButtons(ns("dce_effects"), "Effects to be estimated", 
                     choices = c("Main effects only" = "me", 
                                 "Main effects plus all two factor interactions" = "me_all_2fi", 
                                 "Main effects plus selected two factor interactions" = "me_sel_2fi")),
        conditionalPanel('input.dce_effects == "me_sel_2fi"', ns = ns,
                         width = 12,
                         strong("Enter the two factor interactions here:"),
                         p("One interaction per row, right click on table to add or remove rows."),
                         rHandsontableOutput(ns("hot_ints"))),
        br(),
        actionButton(inputId = ns("reset"), label = "Reset"),
        actionButton(inputId = ns("perform_search"), label = "Perform Search"),
        h1(textOutput(ns("display_output")), style = "font-size:0.1px")
    ),
    
    column(width = 8,
           # Display the assessment of the design 
           conditionalPanel('output.display_output == "TRUE"', ns = ns, 
                            box(title = "Design Assessment", width = 12,
                                status = "primary", solidHeader = T,
                                p('The choice sets for this design is:'),
                                uiOutput(ns('csets'), align = 'center') %>% withSpinner(),
                                p('The information matrix for this design is:'),
                                uiOutput(ns('FImatrix'), align = 'center') %>% withSpinner(),
                                htmlOutput(ns("designInfo")) %>% withSpinner()
                            )
           )
    )
  )
  # End of UI
}

# Function for module server logic
find_design <- function(input, output, session) {
  
  # Create variables for hiding choice set entry and assessment display
  values = reactiveValues(
    reset_val = 0,
    display_output = 'FALSE'
  )
  
  # Toggle assessment display
  output$display_output <- renderText({invisible(values$display_output)})
  observeEvent(input$perform_search, {values$display_output <- 'TRUE'})
  observeEvent(input$reset, {values$display_output <- 'FALSE'})
  
  
  # DEFINING LEVELS-----
  # Define a table containing the number of levels for each factor
  myDF <- reactive({
    DF <- NULL
    if (!is.null(input$hot)){ #If hot changes, 
      DF <- hot_to_r(input$hot) #then update DF and also update DF in values,
      values[['DF']] <- DF
    } else if(!is.null(isolate(values$DF))){ #otherwise use the DF in values.
      DF <- isolate(values$DF)
    } else {
      DF <- data.frame(NumLevels = as.integer(rep(2,3)))
      values[['DF']] <- DF
      values$reset_val <- 1
    }
    DF
  }) %>% debounce(1000)
  
  # Create handsontable to display levels on dashboard
  output$hot = renderRHandsontable({
    if (isolate(values$reset_val)){
      DF = values$DF
    } else DF = myDF()
    if (!is.null(DF)) rhandsontable(DF)
  })
  
  # DEFINING INTERACTIONS-----
  # Define a table containing the interactions that we want to estimate
  int_table <- reactive({
    int_tab <- NULL
    if (!is.null(input$hot_ints)){ #If hot changes,
      int_tab <- hot_to_r(input$hot_ints) #then update int_tab and also update int_tab in values,
      values[['int_tab']] <- int_tab
    } else if(!is.null(isolate(values$int_tab))){ #otherwise use the int_tab in values.
      int_tab <- isolate(values$int_tab)
    } else {
      int_tab <- data.frame(Factor1 = c(1),
                            Factor2 = c(2))
      values[['int_tab']] <- int_tab
      values$reset_val <- 1
    }
    int_tab
  }) %>% debounce(1000) 
  
  # Create handsontable to display selected interactions on dashboard
  output$hot_ints = renderRHandsontable({
    if (isolate(values$reset_val)){
      int_tab = values$int_tab
    } else int_tab = int_table()
    if (!is.null(int_tab)) rhandsontable(int_tab)
  })
  
  
  # GENERATE DESIGN ASSESSMENT-----
  # Call functions in CommonFunctions.R to assess the enteres design
  assessDesign <- eventReactive(input$perform_search, {
    validate(need(!anyNA(values$DF[,1]),"The set of levels contains NA values"))
    
    levels <- values$DF[,1]
    if(input$dce_effects == 'me'){
      find_design_exchange(levels, num_opt, num_runs, num_iter, interactions = NULL, prec = 1e-10)
    } else if (input$dce_effects == 'me_all_2fi'){
      find_design_exchange(levels, num_opt, num_runs, num_iter, interactions = 'all', prec = 1e-10)
    } else if (input$dce_effects == 'me_sel_2fi'){
      find_design_exchange(levels, num_opt, num_runs, num_iter, interactions = int_table(), prec = 1e-10)
    }
  })
  
  # Display Choice Sets
  output$csets <- renderTable({
    ad <- assessDesign()
    ad$choicesets
  }, colnames = F, digits = 0)
  
  # Display Information Matrix
  output$FImatrix <- renderTable({
    ad <- assessDesign()
    ad$cmat
  }, colnames = F)
  
  # Display design assessment
  output$designInfo <- renderText({
    ad <- assessDesign()
    
    if(input$dce_effects == 'me'){
      HTML(paste0("The determinant of the information matrix is: <b>",signif(ad$detmatc, 4), 
                  "</b><br/>The efficiency compared with complete factorial (optimal): <b>",
                  signif(ad$efficiency,4), '%<b/>'))
    } else {
      HTML(paste0("The determinant of the information matrix is: <b>",
                  signif(ad$detmatc, 4)))
    } 
  })
  
  # End of Server
}
