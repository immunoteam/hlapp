selectColumnsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    p(texts$select_main),
    # verbatimTextOutput(ns("test")),
    bslib::layout_columns(
      
      #id column ui
      bslib::card(
        tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
          p(texts$select_id), 
          bslib::tooltip(bsicons::bs_icon("info-circle"), texts$select_id_tooltip),
        ),
        shiny::uiOutput(ns("id_pre_check") ) , # repurposed, now checks if there are ANY colums that qualify as IDs, prior to selection 
        shiny::selectInput(ns("idcol"), NULL, choices = character(1) ) ),
      
      # hla column ui
      bslib::card( 
        tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
                  p(texts$select_hla), 
                  bslib::tooltip(bsicons::bs_icon("info-circle"), texts$select_hla_tooltip),
        ),
        shiny::uiOutput(ns("hla_pre_check")),
        shiny::checkboxGroupInput(ns("hlacol"), NULL, choices = character() ),
        shiny::uiOutput(ns("select_check") )
      )
    ),
    
    #nav buttons ui
    bslib::layout_columns(
      shiny::actionButton(ns("select_back"), "Go back", disabled = FALSE),
      shiny::actionButton(ns("select_next"), "Next ➡️", disabled = TRUE),
      col_widths = c(2, -6, 2, -2)
    )
    # ,verbatimTextOutput(ns("test")) # for testing
    
  )
}

selectColumnsServer <- function(id, input_df = NULL, main_session){
  moduleServer(id, function(input, output, session) {
    
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
    
    #nav buttons
    observeEvent(input$select_back, {
      updateTabsetPanel(main_session, "main_tabs", selected = "upload")
    })
    
   
    observeEvent(select_check(), {
      shiny::updateActionButton(session, "select_next", disabled = !select_check())
    })
    
    observeEvent(input$select_next, {
      updateTabsetPanel(main_session, "main_tabs", selected = "result")
    })
    
    # SERVER RESULT ---------------------------------------
    # return middle table 
    middle_table <- eventReactive(input$select_next, {
      formatInputTable(input_df(), manual = TRUE, 
                       id.col = input$idcol, hla.cols = input$hlacol,
                       flag.forbidden = T ) |>
        joinGenotypePromiscuity(promiscuity) |>
        dplyr::mutate_if(is.numeric, signif, 4)
    })
    
    ### TESTING
    # output$test  <- renderPrint(list(input$idcol, length(input$hlacol), select_check()))
    
    return(middle_table)
  })
  
}