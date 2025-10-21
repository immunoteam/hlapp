# --- Upload UI ---

uploadUI <- function(id){
  ns <-NS(id)
  fluidRow(  
    shiny::fileInput(ns( "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")), 
    shiny::uiOutput(ns( "file_message")), 
    shiny::uiOutput(ns( "file_next"))
  )
}

# ---Select_columns_UI---

# select columns 
selectUI <- function(id){
  ns <- NS(id)
  fluidRow(
    shiny::radioButtons(ns("is_manual"),  "Input recognition:", choices = c("Automatic" = FALSE, "Manual" = TRUE)),
    shiny::tabsetPanel(
      id = ns("input_switcher"), type = "hidden",
      shiny::tabPanelBody("automatic", ""),
      shiny::tabPanelBody("manual",         
                          shiny::selectInput(ns("idvar"), "ID column:", choices = character(1) ),
                          shiny::checkboxGroupInput(ns("hlavar"), "HLA genotype columns:", choices = character() ))
    ),
    shiny::actionButton(ns("next_button") , "Next!", disabled = T)
  )
}

chooseUI <- function(id){
  ns<- NS(id)
  fluidRow(
    shiny::p("Select the promiscuity values to be calculated!"),
    
    shiny::checkboxGroupInput(ns("type"), "Promiscuity to calculate:", 
                              choices = checkbox_options,
                              selected = c("Aa", "Ba", "Ca", "DPa", "DQa", "DRa")),
    shiny::actionButton(ns("submit"),  "Get results")  # submit input for promiscuity calculation
  )
}

resultsUI <- function(id){
  ns <- NS(id)
  shiny::fluidRow(
    shiny::fluidRow(
      shiny::column(6, shiny::h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(ns("middle_table") ) ), # middle table containing promiscuities of individual alleles
      shiny::column(6, shiny::h4("Patient promiscuity:"), DT::dataTableOutput(ns("prom_table") ) ) # final output table 
    ),
    shiny::fluidRow(
      shiny::downloadButton(ns("download_all"), "Download all") # TODO - make download button only appear after results are shown
    )
  ) 
}