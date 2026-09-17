selectColumnsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    p(texts$select_main),
    verbatimTextOutput(ns("test")),
    bslib::layout_columns(
      
      #id column ui
      bslib::card(
        tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
          p(texts$select_id), 
          bslib::tooltip(bsicons::bs_icon("info-circle"), texts$select_id_tooltip)
        ),
        shiny::uiOutput(ns("id_pre_check") ) , # repurposed, now checks if there are ANY colums that qualify as IDs, prior to selection 
        shiny::selectInput(ns("idcol"), NULL, choices = character(1) ) ),
      
      # hla column ui
      bslib::card( 
        tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
                  p(texts$select_hla), 
                  bslib::tooltip(bsicons::bs_icon("info-circle"), texts$select_hla_tooltip)
        ),
        shiny::uiOutput(ns("hla_pre_check")),
        shiny::checkboxInput(ns("hlacol_all"), "Select / Deselect all", value = FALSE), 
        shiny::checkboxGroupInput(ns("hlacol"), NULL, choices = character() ),
        shiny::uiOutput(ns("select_check") ),
        shiny::radioButtons(ns("delim"), label = "Select delimiter (if multiple alleles are stored in a column)",
                            choiceNames = c("\",\"", "\"space\"", "\";\""),
                            choiceValues = c(",", " ", ";"), 
                            selected = ",",
                            inline = T),
        shiny::uiOutput(ns("allele_check") )
      )
    ),
    
    #nav buttons ui
    bslib::layout_columns(
      shiny::actionButton(ns("select_back"), "Go back", disabled = FALSE),
      shiny::uiOutput(ns("select_next_ui")),
      col_widths = c(2, -6, 2, -2)  
    ) 
     ,verbatimTextOutput(ns("test")) # for testing
    
  )
}

selectColumnsServer <- function(id, input_df = NULL, prom_session){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # get the column names of input_df
    cols <- reactive(  checkDfCols(input_df() ) )
    
    # CHECKPOINTS --------------------------------------------------------------------
    # check if there is at least one column, that qualifies as ID column
    id_pre_check <- eventReactive(cols(),  length(cols()[[1]] ) > 0 )
    
    #check if any HLA columns are detected
    hla_pre_check <- eventReactive(cols(),  length(cols()[[2]] ) > 0 )
    
    # check if id and at least one hla column is selected
    select_check <- reactive( input$idcol != "" & length(input$hlacol) > 0  )
    
    
    
    # UI ELEMENTS -------------------------------------------------------------
    # ID COLUMN
    output$id_pre_check <- renderUI(giveFeedBack(id_pre_check(), texts$select_id_pass, texts$select_id_fail  ) ) # feedback for detected id columns
    
    observeEvent(cols(), { # update the column with patient IDs, when input_df is changed
     updateSelectInput(session, "idcol", choices = cols()[[1]] ) 
    })
    

    # HLA COLUMN
    # update hla column options when id col is selected
    observeEvent(cols(), {
      updateCheckboxGroupInput(session, "hlacol", choices = cols()[[2]] ) 
    })
    
    output$hla_pre_check <- renderUI(giveFeedBack(hla_pre_check(), texts$select_hla_pass, texts$select_hla_fail)) # feedback for recognised HLA loci
    output$select_check <- renderUI(giveFeedBack(select_check() , texts$select_check_pass , texts$select_check_fail ))
    
    # Update select cool, when selesct /deselect is checked. 
    observeEvent(input$hlacol_all, {
      if (input$hlacol_all) {
        updateCheckboxGroupInput( session, "hlacol", selected = cols()[[2]])
      } else {
        updateCheckboxGroupInput(session, "hlacol", selected = character(0))
      }
    })
    
    #nav buttons
    observeEvent(input$select_back, {
      updateTabsetPanel(prom_session, "prom_tabs", selected = "upload")
    })
    
    observeEvent(input$select_next, {
      updateTabsetPanel(prom_session, "prom_tabs", selected = "result")
    })

    # NEXT nav button - only active if select_check is passed
    output$select_next_ui  <- renderUI(
      if (!(select_check() & allele_check() )) {
        shiny::actionButton(ns("select_next"), "Next ->", disabled = TRUE)
      } else {
        bslib::input_task_button(ns("select_next"), label = "Next -> ", status = "primary" , spinner = TRUE, state = "ready")
      }
    )

    #check if middle table contains recognizeable alleles (at least for each sample)
    
    allele_check <- reactive( {
      input_df() |>
        head(n = 3)|>
        formatInputTable(id.col = input$idcol, hla.cols = input$hlacol,
                         flag.forbidden = T , delim = input$delim) |> 
        group_by(ID) |> 
        summarise(allele_check = any(HLA %in% unlist(allele_names))) |>
        pull(allele_check) |> 
        all()
    })
    
    output$allele_check <- renderUI(giveFeedBack(allele_check() & select_check(), "Alleles recognised",   "No alleles recognised with these settings"))
    
    
    # SERVER RESULT ---------------------------------------
    # return middle table 
    middle_table <-reactiveVal(NULL)
    
    observeEvent(input$select_next, {
      req(input$select_next)
      
      #Sys.sleep(3) # for testing
      out <- formatInputTable(input_df(),  
                              id.col = input$idcol, hla.cols = input$hlacol,
                              flag.forbidden = T , delim = input$delim) |>
        joinGenotypePromiscuity(promiscuity) |>
        dplyr::mutate_if(is.numeric, signif, 4)
      
      middle_table(out)
    })
    
    
    
    ### TESTING
     output$test  <- renderPrint(list(select_check(), allele_check() )  )
    
    return(middle_table)
  })
  
}