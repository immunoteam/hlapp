

promiscuityUI <- function(id) {
  tagList(
    fluidRow(
      fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")),
      textOutput(NS(id, "resptext") ) ,
      actionButton(NS(id, "submit"),  "Submit file")
    ),
    fluidRow(
      column(6, h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(NS(id, "joined_table") ) ),
      column(6, h4("Patient promiscuity:"), DT::dataTableOutput(NS(id, "prom_table") ) )
    )
  )
}
