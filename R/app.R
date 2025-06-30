library(shiny)

promiscuityApp <- function(...){
  ui <- shiny::fluidPage(promiscuityUI("test"))
  
  server <- function(input, output, session) {
    promiscuityServer("test")
  }
  
  shiny::shinyApp(ui, server, ...)
}

#promiscuityApp()