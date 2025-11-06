# ui render function
# Bejelentkezési képernyő UI-ja

loginUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("login_ui"))
}


loginServer <- function(id){
  moduleServer(id, function(input, output, session){
    
    user_data <- reactiveValues(loggedIn = FALSE) # reactive value indicating it the login UI needs to be shown or not
    
    # ui of login interface
    output$login_ui <- renderUI({
      if (!user_data$loggedIn) {
        tagList(
          h5(texts$login_title),
          textInput( NS(id, "userName"), "User Name:"),
          passwordInput( NS(id, "passwd"), "Password:"),
          actionButton( NS(id, "Login"), "Enter"),
          textOutput( NS(id, "login_message")),
          header = HTML("&nbsp;&nbsp;"),
          p(texts$login_footer), 
        )
      } else {
        return(NULL)  # do not show interface if already logged in
      }
    })
    
    # check is password is correct - this needs to be made safe !!!
    login_pass <- eventReactive(input$Login, {
      input$userName == Sys.getenv("userid") && input$passwd == Sys.getenv("pwd")
    })
    
    # feedback message for user about login
    log_mess <-eventReactive(input$Login, {
        ifelse(login_pass(), "", "Failed Login, Please try angain!")
    })
    
   output$login_message <- renderText(log_mess()) # render login message
    
    observeEvent(input$Login, {
      user_data$loggedIn <- login_pass() # update login status
    })
    
    return(login_pass) # server module output, taken by other module
    
  })
}

