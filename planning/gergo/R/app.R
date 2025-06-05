library(shiny)

promiscuityApp <- function(...){
  ui <- fluidPage(promiscuityUI("test"))
  
  server <- function(input, output, session) {
    promiscuityServer("test")
  }
  
  shinyApp(ui, server, ...)
}

#promiscuityApp()