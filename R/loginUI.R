# ui render function


# Bejelentkezési képernyő UI-ja

loginUI <- function(id){
uiOutput(NS( id, "login_ui"))
}

loginServer <- function(id){
  moduleServer(id, function(input, output, session){
    
    user_data <- reactiveValues(loggedIn = FALSE) # reactive value indicating it the login UI needs to be shown or not
     
    # ui of login interface
    output$login_ui <- renderUI({
      if (!user_data$loggedIn) {
        tagList(
          textInput( NS(id, "userName"), "User Name:"),
          passwordInput( NS(id, "passwd"), "Password:"),
          actionButton( NS(id, "Login"), "LogIn"),
          textOutput( NS(id, "login_message"))
        )
      } else {
        return(NULL)  # do not show interface if already logged in
      }
    })
    
    # check is password is correct - this needs to be made safe !!!
    login_pass <- eventReactive(input$Login, {
      input$userName == "test" && input$passwd =="test"
    })
    
    # feedback message for user about login
    log_mess <-eventReactive(input$Login, {
      paste(
        "input received",
        ifelse(login_pass(), "login succesful", "login failed")
      )
    })
    
    output$login_message <- renderText(log_mess()) # render login message
    
    observeEvent(input$Login, {
      user_data$loggedIn <- login_pass() # update login status
    })
    
    reactive(login_pass()) # server module output, taken by other module
    
  })
}


