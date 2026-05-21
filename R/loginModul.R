# ui render function
# Bejelentkezési képernyő UI-ja

loginUI <- function(id){
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("login_ui"))
}


loginServer <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    
    user_data <- shiny::reactiveValues(loggedIn = FALSE) # reactive value indicating it the login UI needs to be shown or not
    
    # ui of login interface
    output$login_ui <- shiny::renderUI({
      if (!user_data$loggedIn) {
        shiny::tagList(
          shiny::h5(texts$login_title),
          shiny::textInput( shiny::NS(id, "userName"), "User Name:"),
          shiny::passwordInput( shiny::NS(id, "passwd"), "Password:"),
          shiny::actionButton( shiny::NS(id, "Login"), "Enter"),
          shiny::textOutput( shiny::NS(id, "login_message")),
          header = shiny::HTML("&nbsp;&nbsp;"),
          shiny::p(texts$login_footer), 
        )
      } else {
        return(NULL)  # do not show interface if already logged in
      }
    })
    
    # check is password is correct - this needs to be made safe !!!
    login_pass <- shiny::eventReactive(input$Login, {
      input$userName == Sys.getenv("userid") && input$passwd == Sys.getenv("pwd")
    })
    
    # feedback message for user about login
    log_mess <-shiny::eventReactive(input$Login, {
         ifelse(login_pass(), "", "Failed Login, Please try angain!")
    })
    
   output$login_message <- shiny::renderText(log_mess()) # render login message
    
    shiny::observeEvent(input$Login, {
      user_data$loggedIn <- login_pass() # update login status
    })
    
    return(login_pass) # server module output, taken by other module
    
  })
}
