# translate Modul
# part of the modul or can e run separately in translateApp

translateUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

translateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for Translate App")
  })
}