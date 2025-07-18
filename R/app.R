library(shiny)

promiscuityApp <- function(...){
  ui <- shiny::fluidPage(
    loginUI("login"),
    promiscuityUI("test")
    )
  
  server <- function(input, output, session) {
    login_status <- loginServer("login")
    promiscuityServer("test", login_status = login_status)
  }
  
  shiny::shinyApp(ui, server, ...)
}

