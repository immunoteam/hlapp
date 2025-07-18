
#define ui
promiscuityUI <- function(id) {
  shiny::tagList(
    
    tags$head(tags$meta(name = "robots", content = "noindex, nofollow")), # this line prevents search bots to read the content of the app
    
    shiny::titlePanel("Promiscuity calculation"),
    
    uiOutput(NS(id, "login_feedback")),   
    
    uiOutput(NS(id, "promiscuity_interface")), 

  )
}
