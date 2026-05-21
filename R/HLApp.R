HLApp <- function(...){
  
  ui <- shiny::fluidPage(
    #theme = bs_theme(version = 5),  # Bootstrap 5 theme
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", "card-border-width" = "0",  "card-box-shadow" = "none"),
    shiny::tags$head(shiny::tags$meta(name = "robots", content = "noindex, nofollow")), # this line prevents search bots to read the content of the app
    shiny::titlePanel(texts$main_title),
    shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
    loginUI("login"),
    bslib::page_navbar( navbar_options =  bslib::navbar_options(collapsible = F),
      bslib::nav_panel("PromiscuityApp", value = "prom", shiny::uiOutput("promUI") ), 
      bslib::nav_panel("SubmotifApp", value = "submotif", shiny::uiOutput("submUI") ), 
      bslib::nav_panel("HLA translate", value = "translate", shiny::uiOutput("transUI") ), 
      
      footer = shiny::fluidRow(shiny::HTML("&nbsp;&nbsp;&nbsp;"),
                        shiny::hr(size = 1),
                        shiny::p(texts$app_footer))
    )
    
  )
  
  server <- function(input, output, session) {
    
    login_status <- loginServer("login") #ask for login credentials
    # Give feedback on login
    # output$login_feedback <- shiny::renderText(ifelse(login_status(), "", "Failed Login, Please try angain!"))
    
    
    #### PROMISCUITY
    # render Promiscuuity UI if login is passed
    output$promUI <- shiny::renderUI({
      shiny::req(login_status())
      promiscuityUI("promiscuity")
    })
    
    # load promiscuity server if login is passed
    prom_result <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(login_status(), {
      if (login_status()) {
        prom_result(promiscuityServer("promiscuity"))  
      }
    })
    
    #### SUBMOTIF 
    # render submotif UI if login is passed
    output$submUI <- shiny::renderUI({
      shiny::req(login_status())
      submotifUI("submotif")
    })
    
    # load submotifServer if login is passed
    shiny::observeEvent(login_status(), {
      if (login_status()){
        submotifServer("submotif")
      }
    })
    
    
    #### TRANSLATE 
    # render translate UI if login is passed
    output$transUI <- shiny::renderUI({
      shiny::req(login_status())
      translateUI("translate")
    })
    
    # load translateServer if login is passed
    shiny::observeEvent(login_status(), {
      if (login_status()){
        translateServer("translate", prom_result)
      }
    })
    
  }
  
  shiny::shinyApp(ui, server, ...)
}
