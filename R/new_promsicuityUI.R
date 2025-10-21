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
  resultsUI(id)
  )
}
  