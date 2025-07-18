library(shiny)

promiscuityApp <- function(...){
  ui <- shiny::fluidPage(
    loginUI("login"),
    promiscuityUI("test")
    )
  
  server <- function(input, output, session) {
    loginServer("login")
    promiscuityServer("test")
  }
  
  shiny::shinyApp(ui, server, ...)
}

