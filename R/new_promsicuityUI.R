# new promiscuity UI

promiscuityUI2 <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::h3("1. Choose Input:"),
      bslib::navset_tab(
        bslib::nav_panel( "1. Upload file", 
                          #fileInput(ns("file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx"))
                          uploadModuleUI(ns(id))
        ), 
        bslib::nav_panel( "Select input columns", 
                          shiny::radioButtons(ns("is_manual"),  "Input recognition:", choices = c("Automatic" = FALSE, "Manual" = TRUE)),
                          shiny::tabsetPanel(
                            id = ns("input_switcher"), type = "hidden",
                            shiny::tabPanelBody("automatic", ""),
                            shiny::tabPanelBody("manual",         
                                                shiny::selectInput(ns("idvar"), "ID column:", choices = character(1) ),
                                                shiny::checkboxGroupInput(ns("hlavar"), "HLA genotype columns:", choices = character() ))
                          ),
                          shiny::actionButton(ns("next_button") , "Next!", disabled = T)
        ),
        bslib::nav_panel("Choose output", 
                         shiny::p("Select the promiscuity values to be calculated!"),
                         
                         shiny::checkboxGroupInput(ns("type"), "Promiscuity to calculate:", 
                                                   choices = checkbox_options,
                                                   selected = c("Aa", "Ba", "Ca", "DPa", "DQa", "DRa")),
                         shiny::actionButton(ns("submit"),  "Get results")  # submit input for promiscuity calculation
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
  
}