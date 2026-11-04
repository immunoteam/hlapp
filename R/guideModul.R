# translate Modul
# part of the modul or can e run separately in translateApp

guideUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    #includeHTML("www/html/HLApp_user_guide.html")
    
    tags$iframe(
      src = "html/HLApp_user_guide.html",
      style = "width: 100%; height: 85vh; border: none;",
      title = "HLApp User Guide"
    )
  )
}

guideServer <- function(id, prom_result = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  })
}
