

promiscuityUI <- function(id) {
  tagList(
    titlePanel("Promiscuity calculation"),
    sidebarLayout(
      sidebarPanel(
        fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")),
        textOutput(NS(id, "resptext") ) ,
        radioButtons("delim","Deliminator: ",
                     choices = c(";",",",":"), 
                     selected = ";", inline=TRUE),
        actionButton(NS(id, "submit"),  "Submit file"),
        checkboxGroupInput("type", "Promiscuity to calculate:", 
                           choices = c("HLA-A (individual alleles)", 
                                       "HLA-B (individual alleles)", 
                                       "HLA-C (individual alleles)", 
                                       "HLA-A (average)", "HLA-B (average)",
                                       "HLA-C (average)", "all HLA-I (average)"),
                           selected = c("HLA-A (average)", "HLA-B (average)",
                                        "HLA-C (average)")),
      ),
      mainPanel(
        fluidRow(
          column(6, h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(NS(id, "joined_table") ) ),
          column(6, h4("Patient promiscuity:"), DT::dataTableOutput(NS(id, "prom_table") ) )
        ),
        fluidRow(
          downloadButton("download_all", "Download all") # TODO - make download button only appear after results are shown
        )
      )
    )


  )
}
