selectColumnsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    #shiny::verbatimTextOutput(ns("sub_col_feedback")), # for testing
    bslib::layout_columns(
      shiny::selectInput(ns("idcol"), "ID column:", choices = character(1) ),
      shiny::uiOutput(ns("id_check"))
    ),
    bslib::layout_columns(
      shiny::checkboxGroupInput(ns("hlacol"), "HLA genotype columns:", choices = character() ),
      shiny::uiOutput(ns("hla_check"))
    )
    
    #, DT::dataTableOutput(ns("mid_tab")),
  )
}

selectColumnsServer <- function(id, input_df = NULL, main_session){
  moduleServer(id, function(input, output, session) {
    
    # get the column names of input_df
    cols <- reactive(colnames(input_df()))
    # output$sub_col_feedback <- renderPrint(cols()) # for testing
    
    # update the column with patient IDs, when input_df is changed
    observeEvent(cols(), {
      updateSelectInput(session, "idcol", choices = cols() ) 
    })
    
    # check if ID values are unique, and give feedback
    id_check <- eventReactive(input$idcol, {
      ids <- input_df()[[input$idcol]]
      length(ids) == dplyr::n_distinct(ids)
    })
    output$id_check <- renderUI(giveFeedBack(id_check())) # feedback for unique ids
    
    # update hla column options when id col is selected
    observeEvent(input$idcol, {
      updateCheckboxGroupInput(session, "hlacol", choices = cols()[cols() != input$idcol] ) 
    })
    
    
    middle_table <- eventReactive(input$hlacol, {
      formatInputTable(input_df(), manual = TRUE, 
                       id.col = input$idcol, hla.cols = input$hlacol,
                       flag.forbidden = T ) |>
        joinGenotypePromiscuity(promiscuity) |>
        dplyr::mutate_if(is.numeric, signif, 4)
    })
    
    #check if all HLA loci are recognised
    hla_check <- eventReactive(input$hlacol, {
      all(middle_table()[["Locus"]] %in% c("A", "B", "C", "DP", "DQ", "DR"))
    })
    output$hla_check <- renderUI(giveFeedBack(hla_check())) # feedback for recognised HLA loci
    
    #output$mid_tab <- DT::renderDT(middle_table())
    
    return(middle_table)
  })
  
}