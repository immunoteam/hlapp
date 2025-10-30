# new promiscuity UI

promiscuityUI <- function(id){
  ns <- NS(id)
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::h3("1. Choose Input:"),
      bslib::navset_tab(
        bslib::nav_panel( "1. Upload file", 
                          #fileInput(ns("file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx"))
                          #uploadUI(id)
                          uploadUI(ns("upload"))
        ), 
        bslib::nav_panel( "Select input columns", 
                         #selectUI(id)
                         selectColumnsUI(ns("columns"))
        ),
        bslib::nav_panel("Choose output", 
                         #chooseUI(id)
        )
      )
    ),
    
    ### OUTPUT PANELS
    h3("input_colnames"), 
    shiny::verbatimTextOutput(ns("main_col_feedback")),
    
    h3("results placeholder"),
    DT::DTOutput(ns("df_head"))
    #resultsUI(id)
  )
}
  
