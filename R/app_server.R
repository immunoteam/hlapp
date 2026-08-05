
#' main server
#' @param input input
#' @param output output
#' @param session session
#' 
#' @export
HLApp_server <- function(input, output, session) {
  login_status <- loginServer("login")
  
  output$promUI <- shiny::renderUI({
    shiny::req(login_status())
    promiscuityUI("promiscuity")
  })
  
  prom_result <- shiny::reactiveVal(NULL)
  
  output$submUI <- shiny::renderUI({
    shiny::req(login_status())
    submotifUI("submotif")
  })
  
  shiny::observeEvent(login_status(), {
    if (login_status()) {
      subm_session <- submotifServer("submotif")
      prom_result(promiscuityServer("promiscuity", app_session = session, subm_session = subm_session))
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