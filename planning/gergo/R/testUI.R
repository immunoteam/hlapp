

histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist")),
    #addon
    fileInput(NS(id, "file"), accept = c(".csv"), label = "add file"),
    actionButton(NS(id, "submit"),  "Submit file"),
    tableOutput(NS(id, "table")),
    tableOutput(NS(id, "prom"))
  )
}