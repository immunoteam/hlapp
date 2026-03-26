#uploadModule

uploadUI <- function(id){
  ns <- NS(id)
  tagList(
    p(texts$upload_main),
    tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
      shiny::fileInput(ns("file"), 
                       label = "", 
                       accept = c(".tsv", ".csv", ".xlsx") ),
      bslib::tooltip(bsicons::bs_icon("info-circle"), 
                     texts$upload_tooltip),
    ),
    shiny::uiOutput(ns("file_feedback")),
    bslib::layout_columns(
      shiny::actionButton(ns("upload_next"), "Next ➡️", disabled = TRUE),
      col_widths = c(2, -10)
    )
  )
}

uploadServer <- function(id, main_session){
  moduleServer(id, function(input, output, session) {
    #take the selected file, if any, if no file is selected, dont do anything
    input_file <- reactive({
      validate(need(input$file, message = F))
      input$file
    })
    
    input_tmp <- reactive({ # store input in tmp object first
      readInputFile(input_file()) 
    })
    
    file_check <- reactive(is.data.frame(input_tmp())) # check if inout could be read in as a data frame
    output$file_feedback <- renderUI(giveFeedBack(file_check(), texts$upload_feedback_pass, texts$upload_feedback_fail ) )
    
    observeEvent(file_check(), { # allow moving to next step if input is a data frame
      shiny::updateActionButton(session, "upload_next", disabled = !file_check())
    })
    
    input_df <- eventReactive(input$upload_next,  { # if next is clicked store inout data frame
      input_tmp()
    })
    
    observeEvent(input$upload_next, { #if next is clicked, move to select panel
      updateTabsetPanel(main_session, "prom_tabs", selected = "select")
    })
    
    return(input_df)
  })
}
