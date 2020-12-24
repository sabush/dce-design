# Function for module UI
construct_designUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    box(
      width = 4, title = "Design Dimensions",
      status = "primary", solidHeader = T,
      strong("Enter the number of levels for each factor:"),
      p("One factor per row, right click on table to add or remove rows."),
      rHandsontableOutput(ns("hot")),
      br(),
      numericInput(ns("num_opt"), "Number of options per choice set", 2,
        min = 2,
        max = 8
      ),
      radioButtons(ns("dce_effects"), "Effects to be estimated",
        choices = c(
          "Main effects only" = "me",
          "Main effects plus all two factor interactions" = "me_all_2fi",
          "Main effects plus selected two factor interactions" = "me_sel_2fi"
        )
      ),
      conditionalPanel('input.dce_effects == "me_sel_2fi"',
        ns = ns,
        width = 12,
        strong("Enter the two factor interactions here:"),
        p("One interaction per row, right click on table to add or remove rows."),
        rHandsontableOutput(ns("hot_ints"))
      ),
      br(),
      actionButton(inputId = ns("enter_levels"), label = "Enter Generators"),
      actionButton(inputId = ns("reset"), label = "Reset"),
      h1(textOutput(ns("enter_design")), style = "font-size:0.1px"),
      h1(textOutput(ns("display_output")), style = "font-size:0.1px")
    ),

    column(
      width = 8,
      # Enter the generators used to contstruct choice sets
      conditionalPanel('output.enter_design == "TRUE"',
        ns = ns,
        box(
          title = "Enter generators", width = 12,
          status = "primary", solidHeader = T,
          p(HTML("Each row is a single set of generators. Levels are labelled (0,1,...,<i>l</i>), where <i>l</i> is the number of levels for each factor.")),
          p("OptA_FacB corresponds to the level of factor B in the generator for option A of the choice set. The generator for Option 1 is (0,0,...,0)."),
          p("Right click on a cell of the table to add or remove a generator."),
          rHandsontableOutput(ns("hot_gens")),
          br(),
          actionButton(inputId = ns("test_design"), label = "Test Design")
        )
      ),
      br(),
      # Display the assessment of the design
      conditionalPanel('output.display_output == "TRUE"',
        ns = ns,
        box(
          title = "Design Assessment", width = 12,
          status = "primary", solidHeader = T,
          p("The choice sets for this design is:"),
          uiOutput(ns("csets"), align = "center"),
          p("The information matrix for this design is:"),
          uiOutput(ns("FImatrix"), align = "center"),
          htmlOutput(ns("designInfo"))
        )
      )
    )
  )
  # end UI
}

# Function for module server logic
construct_design <- function(input, output, session) {

  # Create variables for hiding choice set entry and assessment display
  values <- reactiveValues(
    reset_val = 0,
    display_output = "FALSE",
    enter_design = "FALSE"
  )

  # Toggle choice set entry
  output$enter_design <- renderText({
    invisible(values$enter_design)
  })
  observeEvent(input$enter_levels, {
    values$enter_design <- "TRUE"
  })
  observeEvent(input$reset, {
    values$enter_design <- "FALSE"
  })

  # Toggle assessment display
  output$display_output <- renderText({
    invisible(values$display_output)
  })
  observeEvent(input$test_design, {
    values$display_output <- "TRUE"
  })
  observeEvent(input$reset, {
    values$display_output <- "FALSE"
  })


  # DEFINING LEVELS-----
  # Define a table containing the number of levels for each factor
  myDF <- reactive({
    DF <- NULL
    if (!is.null(input$hot)) { # If hot changes,
      DF <- hot_to_r(input$hot) # then update DF and also update DF in values,
      values[["DF"]] <- DF
    } else if (!is.null(isolate(values$DF))) { # otherwise use the DF in values.
      DF <- isolate(values$DF)
    } else {
      DF <- data.frame(NumLevels = as.integer(rep(2, 2)))
      values[["DF"]] <- DF
      values$reset_val <- 1
    }
    DF
  }) %>% debounce(1000)

  # Create handsontable to display levels on dashboard
  output$hot <- renderRHandsontable({
    if (isolate(values$reset_val)) {
      DF <- values$DF
    } else {
      DF <- myDF()
    }
    if (!is.null(DF)) rhandsontable(DF)
  })

  # DEFINING INTERACTIONS-----
  # Define a table containing the interactions that we want to estimate
  int_table <- reactive({
    int_tab <- NULL
    if (!is.null(input$hot_ints)) { # If hot changes,
      int_tab <- hot_to_r(input$hot_ints) # then update int_tab and also update int_tab in values,
      values[["int_tab"]] <- int_tab
    } else if (!is.null(isolate(values$int_tab))) { # otherwise use the int_tab in values.
      int_tab <- isolate(values$int_tab)
    } else {
      int_tab <- data.frame(
        Factor1 = c(1),
        Factor2 = c(2)
      )
      values[["int_tab"]] <- int_tab
      values$reset_val <- 1
    }
    int_tab
  }) %>% debounce(1000)

  # Create handsontable to display selected interactions on dashboard
  output$hot_ints <- renderRHandsontable({
    if (isolate(values$reset_val)) {
      int_tab <- values$int_tab
    } else {
      int_tab <- int_table()
    }
    if (!is.null(int_tab)) rhandsontable(int_tab)
  })

  # DEFINING GENERATORS-----
  # Create a table to define generators
  myDF_gens <- reactive({
    gens <- NULL

    if (!is.null(input$hot_gens)) { # If hot changes,
      gens <- hot_to_r(input$hot_gens) # then update DF and also update DF in values,
      values[["gens"]] <- gens
    } else if (!is.null(isolate(values$gens))) { # otherwise use the DF in values.
      gens <- isolate(values$gens)
    } else {
      # Count the number of rows in the levels vector
      num_fac <- myDF() %>% nrow()

      gens <- data.frame(
        entry = as.integer(rep(
          NA,
          as.numeric(num_fac) *
            (as.numeric(input$num_opt) - 1) * 1
        )),
        entry_opt = paste0("Opt", rep(2:as.numeric(input$num_opt),
          each = as.numeric(num_fac),
          times = 1
        )),
        entry_fac = paste0("Fac", rep(1:as.numeric(num_fac),
          times = (as.numeric(input$num_opt) - 1) * 1
        )),
        chset = as.integer(rep(1:1, each = as.numeric(num_fac) *
          (as.numeric(input$num_opt) - 1))),
        stringsAsFactors = FALSE
      ) %>%
        unite("entry_label", entry_opt:entry_fac) %>%
        pivot_wider(names_from = entry_label, values_from = entry) %>%
        dplyr::select(-chset)
      values[["gens"]] <- gens
      values$reset_val <- 1
    }
    gens
  }) %>% debounce(1000)

  # Initialise choice set table based on information in design dimensions

  observeEvent(input$enter_levels, {
    # Count the number of rows in the levels vector
    num_fac <- myDF() %>% nrow()

    values$gens <- data.frame(
      entry = as.integer(rep(NA, as.numeric(num_fac) *
        (as.numeric(input$num_opt) - 1) * 1)),
      entry_opt = paste0("Opt", rep(2:as.numeric(input$num_opt),
        each = as.numeric(num_fac), times = 1
      )),
      entry_fac = paste0("Fac", rep(1:as.numeric(num_fac),
        times = (as.numeric(input$num_opt) - 1) * 1
      )),
      chset = as.integer(rep(1:1, each = as.numeric(num_fac) *
        (as.numeric(input$num_opt) - 1))),
      stringsAsFactors = FALSE
    ) %>%
      unite("entry_label", entry_opt:entry_fac) %>%
      pivot_wider(names_from = entry_label, values_from = entry) %>%
      dplyr::select(-chset)

    values$reset_val <- 1
  })

  # Create handsontable to display choice sets on dashboard
  output$hot_gens <- renderRHandsontable({
    if (isolate(values$reset_val)) {
      gens <- values$gens
    } else {
      gens <- myDF_gens()
    }
    if (!is.null(gens)) rhandsontable(gens)
  })

  # GENERATE DESIGN ASSESSMENT-----
  # Call functions in CommonFunctions.R to assess the enteres design
  assessDesign <- eventReactive(input$test_design, {
    validate(
      need(!anyNA(values$gens), "The set of generators contains NA values"),
      need(!anyNA(values$DF[, 1]), "The set of levels contains NA values")
    )
    generators <- as.matrix(values$gens)
    levels <- values$DF[, 1]
    assess_design(levels, generators = generators, print_detail = F)

    if (input$dce_effects == "me") {
      assess_design(levels, generators = generators, print_detail = F)
    } else if (input$dce_effects == "me_all_2fi") {
      assess_design(levels,
        generators = generators, print_detail = F,
        interactions = "all"
      )
    } else if (input$dce_effects == "me_sel_2fi") {
      assess_design(levels,
        generators = generators, print_detail = F,
        interactions = int_table()
      )
    }
  })

  # Display Choice Sets
  output$csets <- renderTable(
    {
      ad <- assessDesign()
      ad$choicesets
    },
    colnames = F,
    digits = 0
  )

  # Display Information Matrix
  output$FImatrix <- renderTable(
    {
      ad <- assessDesign()
      ad$cmat
    },
    colnames = F
  )

  # Display Design Assessment
  output$designInfo <- renderText({
    ad <- assessDesign()
    if (input$dce_effects == "me") {
      HTML(paste0(
        "The determinant of the information matrix is: <b>", signif(ad$detmatc, 4),
        "</b><br/>The efficiency compared with complete factorial (optimal): <b>",
        signif(ad$efficiency, 4), "%<b/>"
      ))
    } else {
      HTML(paste0(
        "The determinant of the information matrix is: <b>",
        signif(ad$detmatc, 4)
      ))
    }
  })

  # end Server
}
