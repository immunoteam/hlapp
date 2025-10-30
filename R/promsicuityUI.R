# new promiscuity UI

promiscuityUI <- function(id){
  ns <- NS(id)
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::tabsetPanel( id = ns("main_tabs"), 
        shiny::tabPanel( "1. Upload file", value = "upload",
                          #fileInput(ns("file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx"))
                          #uploadUI(id)
                          uploadUI(ns("upload"))
        ), 
        shiny::tabPanel( "2. Select input columns", value = "select",
                         #selectUI(id)
                         selectColumnsUI(ns("columns"))
        ),
        shiny::tabPanel("3. Choose output", value = "result",
                         resultsUI(ns("results"))
        )
      )
    ),
    
    ### OUTPUT PANELS
    DT::DTOutput(ns("result_df"))
  )
}
  
