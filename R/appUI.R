
#define ui
appUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    
    tags$head(tags$meta(name = "robots", content = "noindex, nofollow")), # this line prevents search bots to read the content of the app
    
    uiOutput(ns( "login_feedback")),   
    
    uiOutput(ns( "promiscuity_interface"))

  )
}
