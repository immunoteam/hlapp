

promiscuityUI <- function(id) {
  tagList(
    fluidRow(
      fileInput(inputId = "file", label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")),
      textOutput("resptext") ,
      actionButton("submit",  "Submit file")
    ),
    fluidRow(
      column(6, h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput("joined_table") ),
      column(6, h4("Patient promiscuity:"), DT::dataTableOutput("prom_table") )
    )
  )
}
