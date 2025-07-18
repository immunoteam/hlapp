# ui render function
loginUI <- function(id){
  tagList(
    textInput( NS(id, "userName"), "User Name:"),
    passwordInput( NS(id, "passwd"), "Password:"),
    actionButton( NS(id, "Login"), "LogIn"),
    textOutput( NS(id, "login_message"))
  )
}

