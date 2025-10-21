# new promiscuity UI

promiscuityUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::h3("1. Choose Input:"),
      bslib::navset_tab(
        bslib::nav_panel( "1. Upload file", 
                          #fileInput(ns("file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx"))
                          uploadUI(id)
        ), 
        bslib::nav_panel( "Select input columns", 
                         selectUI(id)
        ),
        bslib::nav_panel("Choose output", 
                         chooseUI(id)
        )
      )
    ),
    
    ### OUTPUT PANELS
    shiny::fluidRow(
      shiny::column(6, shiny::h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(ns("middle_table") ) ), # middle table containing promiscuities of individual alleles
      shiny::column(6, shiny::h4("Patient promiscuity:"), DT::dataTableOutput(ns("prom_table") ) ) # final output table 
    ),
    shiny::fluidRow(
      shiny::downloadButton(ns("download_all"), "Download all") # TODO - make download button only appear after results are shown
    )
  )
}
  