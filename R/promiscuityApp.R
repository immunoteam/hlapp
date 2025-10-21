library(shiny)

promiscuityApp <- function(...){
  ui <- shiny::fluidPage(
    
    shiny::titlePanel("Promiscuity calculation"),
    loginUI("login"),
    mainUI("promiscuity")
    )
  
  server <- function(input, output, session) {
    login_status <- loginServer("login")
    mainServer("promiscuity", login_status = login_status)
  }
  
  shiny::shinyApp(ui, server, ...)
}

