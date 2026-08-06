
submotifCompUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::titlePanel("HLA submotif comparison")
  verbatimTextOutput(ns("testing"))
  
  }

submotifCompServer <- function(id, compare_values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$testing <- renderPrint({
      reactiveValuesToList(compare_values)
    })
  })
}