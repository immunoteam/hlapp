
# R/app_server.R
HLApp_server <- function(input, output, session) {
  login_status <- loginServer("login")
  
  output$promUI <- shiny::renderUI({
    shiny::req(login_status())
    promiscuityUI("promiscuity")
  })
  
  prom_result <- shiny::reactiveVal(NULL)
  
  shiny::observeEvent(login_status(), {
    if (login_status()) {
      prom_result(promiscuityServer("promiscuity"))
    }
  })
  
  output$submUI <- shiny::renderUI({
    shiny::req(login_status())
    submotifUI("submotif")
  })
  
  shiny::observeEvent(login_status(), {
    if (login_status()) {
      submotifServer("submotif")
    }
  })
  
  output$transUI <- shiny::renderUI({
    shiny::req(login_status())
    translateUI("translate")
  })
  
  shiny::observeEvent(login_status(), {
    if (login_status()) {
      translateServer("translate", prom_result)
    }
  })
}