
submotifCompUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::titlePanel("HLA submotif comparison")
  }

submotifCompServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}