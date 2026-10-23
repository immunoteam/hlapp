#uploadModule

uploadUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      shiny::p("Upload your file with HLA genotypes. The file should contain:"),
      shiny::tags$ul(
        shiny::tags$li("A column of unique ID values to identify subjects/samples."),
        shiny::tags$li("One or multiple columns with recognizable HLA alleles. One column can contain either a single allele or multiple alleles separated by a delimiter.")
      )
    ),
    shiny::tags$div( style = "display: inline-flex; align-items: center; gap: 8px;",
      shiny::fileInput(ns("file"), 
                       label = "", 
                       accept = c(".tsv", ".csv", ".xlsx", ".txt", ".xls") ),
      bslib::tooltip(bsicons::bs_icon("info-circle"), 
                     texts$upload_tooltip),
      downloadButton(ns("download_example"), "Download example data"),
      uiOutput(ns("file_delim_ui"))
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
    ns <- session$ns
    
    output$download_example<- downloadHandler(
      filename = function() {
        "HLApp_example_input.csv"
      },
      content = function(file) {
        write.csv(HLApp_example_input_data, file, row.names = FALSE)
      }
    )
    
    #take the selected file, if any, if no file is selected, dont do anything
    input_file <- shiny::reactive({
      shiny::validate(shiny::need(input$file, message = F))
      input$file
    })
    
    
    output$file_delim_ui <- renderUI({
      req(input_file())
      file_ext <- tools::file_ext(input_file()$name)
      if (file_ext == "txt"){
        radioButtons(ns("file_delim"), label = "txt detected, select separator", 
                     choices = c("tab" = "\t", "comma" = ",",  "space" = " "), selected = "tab")
      } 
      
    })
    
    input_tmp <- shiny::reactive({ # store input in tmp object first
      readInputFile(input_file(), delim = input$file_delim) 
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
