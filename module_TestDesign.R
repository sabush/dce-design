# Function for module UI
test_designUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Enter the number of levels for each factor, the number of options for each
    # choice set and the effects to be estimated
    box(width = 4,title = "Design Dimensions", status = "primary", solidHeader = T,
        strong("Enter the number of levels for each factor:"),
        rHandsontableOutput(ns("hot")),
        br(),
        numericInput(ns("num_opt"), "Number of options per choice set", 2, min = 2,
                     max = 8),
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
        actionButton(inputId = ns("enter_levels"), label = "Enter Choice Sets"),
        actionButton(inputId = ns("reset"), label = "Reset"),
        h1(textOutput(ns("enter_design")), style = "font-size:0.1px"),
        h1(textOutput(ns("display_output")), style = "font-size:0.1px")
    ),
    
    column(width = 8,
           # Enter the choice sets
           conditionalPanel('output.enter_design == "TRUE"', ns = ns,
                            box(title = "Enter choice sets", width = 12, 
                                status = "primary", solidHeader = T,
                                p(HTML("Each row is a single choice set. Levels are labelled (0,1,...,<i>l</i>), where <i>l</i> is the number of levels for each factor.")),
                                p("OptA_FacB corresponds to the level of factor B in option A of the choice set. "),
                                p("Right click on a cell of the table to add or remove choice sets."),
                                rHandsontableOutput(ns("hot_chset")), 
                                actionButton(inputId = ns("test_design"), label = "Test Design")
                            )
           ),
           br(),
           # Display the assessment of the design 
           conditionalPanel('output.display_output == "TRUE"', ns = ns, 
                            box(title = "Design Assessment", width = 12,
                                status = "primary", solidHeader = T,
                                p('The information matrix for this design is:'),
                                uiOutput(ns('FImatrix')),
                                htmlOutput(ns("designInfo"))
                                )
           )
    )
  )
  # End of UI
}

# Function for module server logic
test_design <- function(input, output, session) {
  
  # Create variables for hiding choice set entry and assessment display
  values = reactiveValues(
    reset_val = 0,
    display_output = 'FALSE',
    enter_design = 'FALSE'
  )
  
  # Toggle choice set entry
  output$enter_design <- renderText({invisible(values$enter_design)})
  observeEvent(input$enter_levels, {values$enter_design <- 'TRUE'})
  observeEvent(input$reset, {values$enter_design <- 'FALSE'})
  
  # Toggle assessment display
  output$display_output <- renderText({invisible(values$display_output)})
  observeEvent(input$test_design, {values$display_output <- 'TRUE'})
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
      DF <- data.frame(NumLevels = as.integer(rep(2,2)))
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
  
  # DEFINING CHOICE SETS-----
  # Create a table to define choice sets
  myDF_chset <- reactive({
    chset <- NULL
    
    if (!is.null(input$hot_chset)){ #If hot changes, 
      chset <- hot_to_r(input$hot_chset) #then update DF and also update DF in values,
      values[['chset']] <- chset
    } else if(!is.null(isolate(values$chset))){ #otherwise use the DF in values.
      chset <- isolate(values$chset)
    } else {
      # Count the number of rows in the levels vector
      num_fac <- myDF() %>% nrow()
      
      chset <- data.frame(entry = as.integer(rep(NA,
                                                 as.numeric(num_fac) * 
                                                   as.numeric(input$num_opt) * 4)),
                          entry_opt = paste0('Opt', rep(1:as.numeric(input$num_opt), 
                                                        each = as.numeric(num_fac), 
                                                        times = 4)),
                          entry_fac = paste0('Fac', rep(1:as.numeric(num_fac), 
                                                        times = as.numeric(input$num_opt)* 4)),
                          chset = as.integer(rep(1:4, 
                                                 each = as.numeric(num_fac) * 
                                                   as.numeric(input$num_opt))), 
                          stringsAsFactors = FALSE) %>% 
        unite("entry_label", entry_opt:entry_fac) %>% 
        pivot_wider(names_from = entry_label, values_from = entry) %>% 
        dplyr::select(-chset)     
      values[['chset']] <- chset
      values$reset_val <- 1
    }
    chset
  }) %>% debounce(1000)
  
  # Initialise choice set table based on information in design dimensions
  observeEvent(input$enter_levels, {
    # Count the number of rows in the levels vector
    num_fac <- myDF() %>% nrow()
    
    values$chset <- data.frame(entry = as.integer(rep(NA,
                                                      as.numeric(num_fac) * 
                                                        as.numeric(input$num_opt) * 4)),
                               entry_opt = paste0('Opt', rep(1:as.numeric(input$num_opt), 
                                                             each = as.numeric(num_fac), times = 4)),
                               entry_fac = paste0('Fac', rep(1:as.numeric(num_fac), 
                                                             times = as.numeric(input$num_opt)* 4)),
                               chset = as.integer(rep(1:4, each = as.numeric(num_fac) * 
                                                        as.numeric(input$num_opt))), 
                               stringsAsFactors = FALSE) %>% 
      unite("entry_label", entry_opt:entry_fac) %>% 
      pivot_wider(names_from = entry_label, values_from = entry) %>% 
      dplyr::select(-chset)
    
    values$reset_val <- 1
  })  
  
  # Create handsontable to display choice sets on dashboard
  output$hot_chset = renderRHandsontable({
    if (isolate(values$reset_val)){
      chset = values$chset
    } else chset = myDF_chset()
    if (!is.null(chset)) rhandsontable(chset)
  })
  
  # GENERATE DESIGN ASSESSMENT-----
  # Call functions in CommonFunctions.R to assess the enteres design
  assessDesign <- eventReactive(input$test_design, {
    validate(need(!anyNA(values$chset),"The set of choice sets contains NA values"),
             need(!anyNA(values$DF[,1]),"The set of levels contains NA values"))
    
    choicesets <- as.matrix(values$chset)
    levels <- values$DF[,1]
    if(input$dce_effects == 'me'){
      assess_design_main_effect(choicesets, levels, print_detail = F)
    } else if (input$dce_effects == 'me_all_2fi'){
      assess_design_interactions(choicesets, levels, interactions = 'all', 
                                 print_detail = F)
    } else if (input$dce_effects == 'me_sel_2fi'){
      assess_design_interactions(choicesets, levels, interactions = int_table, 
                                 print_detail = F)
    }
  })
  
  # Display Information Matrix
  output$FImatrix <- renderTable({
    ad <- assessDesign()
    ad$cmat
  }, colnames = F)
  
  # Display design assessment
  output$designInfo <- renderText({
    ad <- assessDesign()
    
    if(input$dce_effects == 'me'){
      HTML(paste0("The determinant of the C matrix is: <b>",signif(ad$detmatc, 4), 
                  "</b><br/>The efficiency compared with complete factorial (optimal): <b>",
                  signif(ad$efficiency,4), '%<b/>'))
    } else {
      HTML(paste0("The determinant of the C matrix is: <b>",
                  signif(ad$detmatc, 4)))
    } 
  })
  
  # End of Server
}