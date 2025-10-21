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

 promiscuityUI_old <- function(id){
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::h3("1. Choose Input:"),
      bslib::navset_tab(
        bslib::nav_panel( "1. Upload file", 
          #fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx"))
          uploadUI(id)
        ), 
        bslib::nav_panel( "Select input columns", 
          shiny::radioButtons(NS(id, "is_manual"),  "Input recognition:", choices = c("Automatic" = FALSE, "Manual" = TRUE)),
          shiny::tabsetPanel(
            id = NS(id, "input_switcher"), type = "hidden",
            shiny::tabPanelBody("automatic", ""),
            shiny::tabPanelBody("manual",         
                         shiny::selectInput(NS(id, "idvar"), "ID column:", choices = character(1) ),
                         shiny::checkboxGroupInput(NS(id, "hlavar"), "HLA genotype columns:", choices = character() ))
          ),
          shiny::actionButton(NS(id, "next_button") , "Next!", disabled = T)
        ),
        bslib::nav_panel("Choose output", 
          shiny::p("Select the promiscuity values to be calculated!"),
          
          shiny::checkboxGroupInput(NS(id, "type"), "Promiscuity to calculate:", 
                             choices = checkbox_options,
                             selected = c("Aa", "Ba", "Ca", "DPa", "DQa", "DRa")),
          shiny::actionButton(NS(id, "submit"),  "Get results")  # submit input for promiscuity calculation
        )
      )
    ),
    
    ### OUTPUT PANELS
      shiny::fluidRow(
        shiny::column(6, shiny::h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(NS(id, "middle_table") ) ), # middle table containing promiscuities of individual alleles
        shiny::column(6, shiny::h4("Patient promiscuity:"), DT::dataTableOutput(NS(id, "prom_table") ) ) # final output table 
      ),
      shiny::fluidRow(
        shiny::downloadButton(NS(id, "download_all"), "Download all") # TODO - make download button only appear after results are shown
      )
    )
}
