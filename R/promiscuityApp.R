library(shiny)

promiscuityApp <- function(...){
  ui <- shiny::fluidPage(
    
    shiny::titlePanel("Promiscuity calculation"),
    loginUI("login"),
    appUI("promiscuity")
    )
  
  server <- function(input, output, session) {
    login_status <- loginServer("login")
    promiscuityServer("promiscuity", login_status = login_status)
  }
  
  shiny::shinyApp(ui, server, ...)
}

