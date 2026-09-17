
#' main server
#' @param input input
#' @param output output
#' @param session session
#' 
#' @export
HLApp_server <- function(input, output, session) {
  # values
  login_status <- loginServer("login")
  prom_result <- shiny::reactiveVal(NULL)
  compare_values <- reactiveValues( alleles = c("A0101", "A0101"))
  
  # ui-s
  output$promUI <- shiny::renderUI({
    shiny::req(login_status())
    promiscuityUI("promiscuity")
  })
  
  
  output$submUI <- shiny::renderUI({
    shiny::req(login_status())
    submotifUI("submotif")
  })
  
  output$submCompUI <- shiny::renderUI({
    shiny::req(login_status())
    submotifCompUI("submotif_comp")
  })
  
  output$transUI <- shiny::renderUI({
    shiny::req(login_status())
    translateUI("translate")
  })
  
  # server logic
  shiny::observeEvent(login_status(), {
    if (login_status()) {
      subm_session <- submotifServer("submotif")
      submotifCompServer("submotif_comp", compare_values)
      prom_result(promiscuityServer("promiscuity", app_session = session, compare_values))
      translateServer("translate", prom_result)
    }
  })
  
 # output$testing <- renderPrint({
 #    reactiveValuesToList(compare_values)
 # })

  
}