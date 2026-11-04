# translate Modul
# part of the modul or can e run separately in translateApp

guideUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    #includeHTML("www/html/HLApp_user_guide.html")
    
    downloadButton(ns("download_guide"), label = "Download user guide"),
    
    tags$iframe(
      src = "guide/HLApp_user_guide.html",
      style = "width: 100%; height: 85vh; border: none;",
      title = "HLApp User Guide"
    )
  )
}

guideServer <- function(id, prom_result = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$download_guide <- downloadHandler(
      filename = function(){"HLApp_user_guide.pdf"}, 
      content = function(file){
        file.copy("www/guide/HLApp_user_guide.pdf", file)
      }
    )
    
  })
}
