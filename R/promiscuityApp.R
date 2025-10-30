library(shiny)

promiscuityApp <- function(...){
  
  ui <- shiny::fluidPage(
    tags$head(tags$meta(name = "robots", content = "noindex, nofollow")), # this line prevents search bots to read the content of the app
    shiny::titlePanel("Promiscuity calculation"),
    loginUI("login"),
    shiny::textOutput("login_feedback"),
    uiOutput("promUI")
  )
  
  server <- function(input, output, session) {
    
    login_status <- loginServer("login") #ask for login credentials
    # Give feedback on login
    output$login_feedback <- renderText(ifelse(login_status(),
                                               "Succesful Login!",
                                               "Failed Login, Please try angain!"))
    
    # render Promiscuuity UI if login is passed
    output$promUI <- renderUI({
      req(login_status())
      promiscuityUI("promiscuity")
    })
    
    # load promiscuity server if login is passed
    observeEvent(login_status(), {
      if (login_status()) {
        promiscuityServer("promiscuity")  
      }
    })
  }
  
  shiny::shinyApp(ui, server, ...)
}
