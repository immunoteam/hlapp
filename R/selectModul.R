selectColumnsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    p(texts$select_main),
    # verbatimTextOutput(ns("test")),
    bslib::layout_columns(
      bslib::card(
        tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
          p(texts$select_id), 
          bslib::tooltip(bsicons::bs_icon("info-circle"), texts$select_id_tooltip),
        ),
        shiny::selectInput(ns("idcol"), NULL, choices = character(1) ),
        shiny::uiOutput(ns("id_check"))  
      ),
      bslib::card( 
        tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
                  p(texts$select_hla), 
                  bslib::tooltip(bsicons::bs_icon("info-circle"), texts$select_hla_tooltip),
        ),
        shiny::checkboxGroupInput(ns("hlacol"), NULL, choices = character() ),
        shiny::uiOutput(ns("hla_check"))
      )
    ),
    
    #navbuttons ui
    bslib::layout_columns(
      shiny::actionButton(ns("select_back"), "Go back", disabled = FALSE),
      shiny::actionButton(ns("select_next"), "Next ➡️", disabled = TRUE),
      col_widths = c(2, -6, 2, -2)
    )
    
    #, DT::dataTableOutput(ns("mid_tab")),
  )
}

selectColumnsServer <- function(id, input_df = NULL, main_session){
  moduleServer(id, function(input, output, session) {
    
    # get the column names of input_df
    cols <- reactive(  checkDfCols(input_df() ) )
    
    # output$test <- renderPrint(cols())
    
    # update the column with patient IDs, when input_df is changed
    observeEvent(cols(), {
     updateSelectInput(session, "idcol", choices = cols()[[1]] ) 
    })
    
    # check if ID values are unique, and give feedback
    id_check <- eventReactive(input$idcol, {
      ids <- input_df()[[input$idcol]]
      length(ids) == dplyr::n_distinct(ids)
    })
    output$id_check <- renderUI(giveFeedBack(id_check())) # feedback for unique ids
    
    # update hla column options when id col is selected
    observeEvent(cols(), {
      updateCheckboxGroupInput(session, "hlacol", choices = cols()[[2]] ) 
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
    
    #navbuttons
    observeEvent(input$select_back, {
      updateTabsetPanel(main_session, "main_tabs", selected = "upload")
    })
    
    select_check <- reactive(id_check() & hla_check())
    observeEvent(select_check(), {
      shiny::updateActionButton(session, "select_next", disabled = !select_check())
    })
    
    observeEvent(input$select_next, {
      updateTabsetPanel(main_session, "main_tabs", selected = "result")
    })
    
    return(middle_table)
  })
  
}