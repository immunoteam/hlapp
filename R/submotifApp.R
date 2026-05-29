# submotif app

submotifApp <- function() {

  ui <- fluidPage(
    tags$head(
      tags$meta(name = "robots", content = "noindex, nofollow")
      ),

    uiOutput("submotifUI")
  )


  server <- function(input, output) {

    output$submotifUI <- renderUI({
      submotifUI("submotif")
    })

    submotifServer("submotif")


  }

  shinyApp(ui = ui, server = server)
}


submotifApp()
