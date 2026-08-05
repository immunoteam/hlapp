#uploadModule

uploadUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(texts$upload_main),
    shiny::tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
      shiny::fileInput(ns("file"), 
                       label = "", 
                       accept = c(".tsv", ".csv", ".xlsx", ".txt", ".xls") ),
      bslib::tooltip(bsicons::bs_icon("info-circle"), 
                     texts$upload_tooltip),
    ),
    shiny::uiOutput(ns("file_feedback")),
    bslib::layout_columns(
      shiny::actionButton(ns("upload_next"), "Next ->", disabled = TRUE),
      col_widths = c(2, -10)
    )
  )
}

uploadServer <- function(id, prom_session){
  shiny::moduleServer(id, function(input, output, session) {
    #take the selected file, if any, if no file is selected, dont do anything
    input_file <- shiny::reactive({
      shiny::validate(shiny::need(input$file, message = F))
      input$file
    })
    
    input_tmp <- shiny::reactive({ # store input in tmp object first
      readInputFile(input_file()) 
    })
    
    file_check <- shiny::reactive(is.data.frame(input_tmp())) # check if inout could be read in as a data frame
    output$file_feedback <- shiny::renderUI(giveFeedBack(file_check(), texts$upload_feedback_pass, texts$upload_feedback_fail ) )
    
    shiny::observeEvent(file_check(), { # allow moving to next step if input is a data frame
      shiny::updateActionButton(session, "upload_next", disabled = !file_check())
    })
    
    input_df <- shiny::eventReactive(input$upload_next,  { # if next is clicked store inout data frame
      input_tmp()
    })
    
    shiny::observeEvent(input$upload_next, { #if next is clicked, move to select panel
      shiny::updateTabsetPanel(prom_session, "prom_tabs", selected = "select")
    })
    
    return(input_df)
  })
}
