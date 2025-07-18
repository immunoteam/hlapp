#' this script creates the main promiscuity app.
#' it was moved here, because it had to be responsive to the login status of the server

#define checkbox optioons outside of ui for easier readibility 
checkbox_options <- c("HLA-A (individual)" = "Ai",
                      "HLA-B (individual" = "Bi",
                      "HLA-C (individual)" = "Ci",
                      "HLA-DP (individual)" = "DPi",
                      "HLA-DQ (individual)" = "DQi",
                      "HLA-DR (individual)" = "DRi",
                      "HLA-A (average)" = "Aa",
                      "HLA-B (average" = "Ba",
                      "HLA-C (average)" = "Ca",
                      "HLA-DP (average)" = "DPa",
                      "HLA-DQ (average)" = "DQa",
                      "HLA-DR (average)" = "DRa",
                      "HLA-I (average)" = "HLA_I",
                      "HLA-II (average)" = 'HLA_II'
)

promiscuityUI2 <- function(id){
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      h3("1. Choose Input:"),
      fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")),
      
      radioButtons(NS(id, "is_manual"),  "Input recognition:", choices = c("Automatic" = FALSE, "Manual" = TRUE)),
      tabsetPanel(
        id = NS(id, "input_switcher"), type = "hidden",
        tabPanelBody("automatic", ""),
        tabPanelBody("manual",         
                     selectInput(NS(id, "idvar"), "ID column:", choices = character(1) ),
                     checkboxGroupInput(NS(id, "hlavar"), "HLA genotype columns:", choices = character() ))
      ),
      
      
      
      # radioButtons("delim","Deliminator: ",  choices = c(";",",",":"), selected = ";", inline=TRUE),
      #DT::dataTableOutput(NS(id, "input_head")), # show input file
      
      h3("2. Choose output:"),
      p("Select the promiscuity values to be calculated!"),
      checkboxGroupInput(NS(id, "type"), "Promiscuity to calculate:", 
                         choices = checkbox_options,
                         selected = c("Aa", "Ba", "Ca", "DPa", "DQa", "DRa")
      ),
      actionButton(NS(id, "submit"),  "Get results")  # submit input for promiscuity calculation
    ),
    mainPanel(
      fluidRow(
        column(6, h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(NS(id, "middle_table") ) ), # middle table containing promiscuities of individual alleles
        column(6, h4("Patient promiscuity:"), DT::dataTableOutput(NS(id, "prom_table") ) ) # final output table 
      ),
      fluidRow(
        downloadButton(NS(id, "download_all"), "Download all") # TODO - make download button only appear after results are shown
      )
    )
  )
}