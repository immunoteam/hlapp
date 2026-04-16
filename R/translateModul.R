# translate Modul
# part of the modul or can e run separately in translateApp

translateUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    textOutput(ns("placeholder")), 
    shiny::verbatimTextOutput(ns("prom_res"))
  )
}

translateServer <- function(id, prom_result = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(prom_result())
      
      data <- prom_result()()  # THIS updates automatically
      # do something with data
      output$prom_res <- shiny::renderPrint(head(data))
    })
    output$placeholder <- renderText("Placeholder for Translate App")
    
    
  })
}