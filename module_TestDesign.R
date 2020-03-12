# Function for module UI
test_designUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    box(width = 4,title = "Design Dimensions",
        numericInput(ns("num_fac"), "Number of factors", 2, min = 2,
                     max = 16),
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
        actionButton(inputId = ns("enter_levels"), label = "Enter Levels"),
        actionButton(inputId = ns("reset"), label = "Reset"),
        h1(textOutput(ns("enter_design")), style = "font-size:0.1px"),
        h1(textOutput(ns("display_output")), style = "font-size:0.1px"),
        h4("Enter the number of levels for each factor:"),
        rHandsontableOutput(ns("hot"))
    ),
    
    column(width = 8,
           conditionalPanel('output.enter_design == "TRUE"', ns = ns,
                            h3("Enter the choice sets here:"),
                            p(HTML("Each row is a single choice set. Levels are labelled (0,1,...,<i>l</i>), where <i>l</i> is the number of levels for each factor.")),
                            p("OptA_FacB corresponds to the level of factor B in option A of the choice set. "),
                            p("Right click on a cell of the table to add or remove choice sets."),
                            rHandsontableOutput(ns("hot_chset")), 
                            actionButton(inputId = ns("test_design"), label = "Test Design")
           ),
           br(),
           conditionalPanel('output.display_output == "TRUE"', ns = ns, 
                            h3("Design Assessment:"),
                            p('The information matrix for this design is:'),
                            uiOutput(ns('FImatrix')),
                            htmlOutput(ns("designInfo"))
           )
    )
    
    # fluidRow(
    #   width = 12,
    #   h3("Design Dimensions"),
    #   column(width = 6,
    #          numericInput(ns("num_fac"), "Number of factors", 2, min = 2,
    #                       max = 16),
    #          numericInput(ns("num_opt"), "Number of options per choice set", 2, min = 2,
    #                       max = 8)),
    #   column(width = 6,
    #          radioButtons(ns("dce_effects"), "Effects to be estimated", 
    #                       choices = c("Main effects only" = "me", 
    #                                   "Main effects plus all two factor interactions" = "me_all_2fi", 
    #                                   "Main effects plus selected two factor interactions" = "me_sel_2fi")),
    #          conditionalPanel('input.dce_effects == "me_sel_2fi"', ns = ns,
    #                           width = 12,
    #                           strong("Enter the two factor interactions here:"),
    #                           p("One interaction per row, right click on table to add or remove rows."),
    #                           rHandsontableOutput(ns("hot_ints"))
    #          )
    #   )),
    # 
    # 
    # fluidRow(
    #   width = 12,
    #   actionButton(inputId = ns("enter_levels"), label = "Enter Levels"),
    #   actionButton(inputId = ns("reset"), label = "Reset"),
    #   h1(textOutput(ns("enter_design")), style = "font-size:0.1px"),
    #   h1(textOutput(ns("display_output")), style = "font-size:0.1px")
    # ),
    # 
    # # Enter choice sets
    # # conditionalPanel('output.enter_design == "TRUE"', ns = ns, 
    # fluidRow(
    #   width = 12,
    #   h3("Design Details"),
    #   column(
    #     width = 4,
    #     h4("Enter the number of levels for each factor:"),
    #     rHandsontableOutput(ns("hot"))
    #   ),
    #   column(
    #     width = 8,
    #     h4("Enter the choice sets here:"),
    #     rHandsontableOutput(ns("hot_chset")), 
    #     p(HTML("Each row is a single choice set. Levels are labelled (0,1,...,<i>l</i>), where <i>l</i> is the number of levels for each factor.")),
    #     p("OptA_FacB corresponds to the level of factor B in option A of the choice set. "),
    #     p("Right click on a cell of the table to add or remove choice sets.")
    #   )
    # ),
    # fluidRow(
    #   width = 12,
    #   actionButton(inputId = ns("test_design"), label = "Test Design"),
    #   br()
    #   # )
    # ),
    # # Display Output
    # conditionalPanel('output.display_output == "TRUE"', ns = ns, 
    #                  width = 12, 
    #                  column(width = 12,
    #                         h3("Design Assessment:"),
    #                         p('The information matrix for this design is:'),
    #                         uiOutput(ns('FImatrix')),
    #                         htmlOutput(ns("designInfo"))
    #                  )
    #                  
    # )
  )
  
}

# Function for module server logic
test_design <- function(input, output, session) {
  
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
  
  
  int_table <- reactive({
    int_tab <- NULL
    if (!is.null(input$hot_ints)){ #If hot changes,
      int_tab <- hot_to_r(input$hot_ints) #then update int_tab and also update int_tab in values,
      values[['int_tab']] <- int_tab
    } else if(!is.null(isolate(values$int_tab))){ #otherwise use the int_tab in values.
      int_tab <- isolate(values$int_tab)
    } else {
      req(input$num_fac)
      int_tab <- data.frame(Factor1 = c(1),
                            Factor2 = c(2))
      values[['int_tab']] <- int_tab
      values$reset_val <- 1
    }
    int_tab
  }) %>% debounce(1000) #debounce slows down so that reactive expression gets less "chatty"
  
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
    validate(need(!anyNA(values$chset),"The set of choice sets contains NA values"),
             need(!anyNA(values$DF[,1]),"The set of levels contains NA values"))
    choicesets <- as.matrix(values$chset)
    levels <- values$DF[,1]
    if(input$dce_effects == 'me'){
      assess_design_main_effect(choicesets, levels, print_detail = F)
    } else if (input$dce_effects == 'me_all_2fi'){
      assess_design_interactions(choicesets, levels, interactions = 'all', print_detail = F)
    } else if (input$dce_effects == 'me_sel_2fi'){
      assess_design_interactions(choicesets, levels, interactions = int_table, print_detail = F)
    }
  })
  
  myDF_chset <- reactive({
    chset <- NULL
    
    if (!is.null(input$hot_chset)){ #If hot changes, 
      chset <- hot_to_r(input$hot_chset) #then update DF and also update DF in values,
      values[['chset']] <- chset
    } else if(!is.null(isolate(values$chset))){ #otherwise use the DF in values.
      chset <- isolate(values$chset)
    } else {
      req(input$num_fac)
      chset <- data.frame(entry = as.integer(rep(NA,as.numeric(input$num_fac) * as.numeric(input$num_opt) * 4)),
                          entry_opt = paste0('Opt', rep(1:as.numeric(input$num_opt), each = as.numeric(input$num_fac), times = 4)),
                          entry_fac = paste0('Fac', rep(1:as.numeric(input$num_fac), times = as.numeric(input$num_opt)* 4)),
                          chset = as.integer(rep(1:4, each = as.numeric(input$num_fac) * as.numeric(input$num_opt))), 
                          stringsAsFactors = FALSE) %>% 
        unite("entry_label", entry_opt:entry_fac) %>% 
        pivot_wider(names_from = entry_label, values_from = entry) %>% 
        dplyr::select(-chset)     
      values[['chset']] <- chset
      values$reset_val <- 1
    }
    chset
  }) %>% debounce(1000) #debounce slows down so that reactive expression gets less "chatty"
  
  output$hot = renderRHandsontable({
    if (isolate(values$reset_val)){
      DF = values$DF
    } else DF = myDF()
    if (!is.null(DF)) rhandsontable(DF)
  })
  
  output$hot_chset = renderRHandsontable({
    if (isolate(values$reset_val)){
      chset = values$chset
    } else chset = myDF_chset()
    if (!is.null(chset)) rhandsontable(chset)
  })
  
  output$hot_ints = renderRHandsontable({
    if (isolate(values$reset_val)){
      int_tab = values$int_tab
    } else int_tab = int_table()
    if (!is.null(int_tab)) rhandsontable(int_tab)
  })
  
  observeEvent(input$enter_levels, {
    req(input$num_fac)
    req(input$num_opt)
    values$DF <- data.frame(NumLevels = as.integer(rep(2,as.numeric(input$num_fac))),
                            row.names = paste('Fac', 1:as.numeric(input$num_fac)))
    
    values$chset <- data.frame(entry = as.integer(rep(NA,as.numeric(input$num_fac) * as.numeric(input$num_opt) * 4)),
                               entry_opt = paste0('Opt', rep(1:as.numeric(input$num_opt), each = as.numeric(input$num_fac), times = 4)),
                               entry_fac = paste0('Fac', rep(1:as.numeric(input$num_fac), times = as.numeric(input$num_opt)* 4)),
                               chset = as.integer(rep(1:4, each = as.numeric(input$num_fac) * as.numeric(input$num_opt))), 
                               stringsAsFactors = FALSE) %>% 
      unite("entry_label", entry_opt:entry_fac) %>% 
      pivot_wider(names_from = entry_label, values_from = entry) %>% 
      dplyr::select(-chset)
    
    # values$data.frame(Factor1 = c(1), Factor2 = c(2))
    
    values$reset_val <- 1
  })  
  
  
  output$FImatrix <- renderTable({
    ad <- assessDesign()
    ad$cmat
  }, colnames = F)
  
  # Show greetings
  output$designInfo <- renderText({
    ad <- assessDesign()
    
    if(input$dce_effects == 'me'){
      HTML(paste0("The determinant of the C matrix is: <b>",signif(ad$detmatc, 4), 
                  "</b><br/>The efficiency compared with complete factorial (optimal): <b>",signif(ad$efficiency,4), '%<b/>'))
    } else {
      HTML(paste0("The determinant of the C matrix is: <b>",signif(ad$detmatc, 4)))
    } 
    
  })
  
}