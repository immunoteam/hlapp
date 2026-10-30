ui <- fluidPage(
  titlePanel("Promiscuity calculation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Select a file with list of the requested alleles",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ),
      radioButtons("separator", "Separator: ",
        choices = c(";", ",", ":"),
        selected = ";", inline = TRUE
      ),
      checkboxGroupInput("type", "Promiscuity to calculate:",
        choices = c(
          "HLA-A (individual alleles)" = "Ai",
          "HLA-B (individual alleles)" = "Bi",
          "HLA-C (individual alleles)" = "Ci",
          "HLA-A (average)" = "Aa", "HLA-B (average)" = "Ba",
          "HLA-C (average)" = "Ca", "all HLA-I (average)" = "I"
        ),
        selected = c("Aa", "Ba", "Ca")
      ),
      radioButtons("NA",
        label = "Ignore missing alleles?",
        choices = list("Calculate with NAs" = T, "Calculate without NAs" = F)
      ),
      downloadButton("download_selected", "Download selected"),
      downloadButton("download_all", "Download all")
    ),
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)
