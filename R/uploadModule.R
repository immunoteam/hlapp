#uploadModule

uploadUI <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fileInput(ns("file"), 
                     label = "Upload HLA genotype file:", 
                     accept = c(".tsv", ".csv", ".xlsx") ),
    shiny::uiOutput(ns("file_feedback")),
    shiny::actionButton(ns("upload_next"), "Next ➡️", disabled = TRUE)
  )
}

uploadServer <- function(id){
  moduleServer(id, function(input, output, session) {
    #take the selected file, if any, if no file is selected, dont do anything
    input_file <- reactive({
      validate(need(input$file, message = F))
      input$file
    })
    
    input_df <- reactive({
      readInputFile(input_file())
    })
    
    file_check <- reactive(is.data.frame(input_df()))
    output$file_feedback <- renderUI(giveFeedBack(file_check()))
    
    observeEvent(file_check(), {
      shiny::updateActionButton(session, "upload_next", disabled = !file_check())
    })
    

    
    return(input_df)
  })
}
