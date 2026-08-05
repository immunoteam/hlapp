# new promiscuity UI

promiscuityUI <- function(id){
  ns <- NS(id)
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::tabsetPanel( id = ns("prom_tabs"), 
                          header = HTML("&nbsp;&nbsp;"),
        shiny::tabPanel( texts$upload_title, value = "upload",
                          #fileInput(ns("file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx"))
                          #uploadUI(id)
                          uploadUI(ns("upload"))
        ), 
        shiny::tabPanel( texts$select_title, value = "select",
                         #selectUI(id)
                         selectColumnsUI(ns("columns"))
        ),
        shiny::tabPanel( texts$result_title, value = "result",
                         resultsUI(ns("results"))
        ), 
        # disacle tabset navigation
        tags$script(HTML("
          $(document).ready(function() {
          $('#prom_tabs ul.nav > li')
            .addClass('disabled')
            .css('pointer-events', 'none');
          });
          "))
      )
    ),
    HTML("&nbsp;&nbsp;&nbsp;"),
    hr(size = 1),
    ### OUTPUT PANELS
    DT::DTOutput(ns("result_df")),
    
    actionButton(ns("funcdiv_button"), label = "Compare functional divergence", disabled = FALSE), # switch to disabled once done with testing
    
    shiny::verbatimTextOutput(ns("clickedval"))
    
  )
}
  
