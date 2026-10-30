# submotif app - for backward compatibility

submotifApp <- function() {
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$meta(name = "robots", content = "noindex, nofollow")
    ),
    shiny::uiOutput("submotifUI")
  )


  server <- function(input, output) {
    output$submotifUI <- shiny::renderUI({
      submotifUI("submotif")
    })

    submotifServer("submotif")
  }

  shiny::shinyApp(ui = ui, server = server)
}


submotifApp()
