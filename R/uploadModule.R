# --- Upload UI ---

uploadUI <- function(id){
  fluidRow(  
    shiny::fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")), 
    shiny::uiOutput(NS(id, "file_message")), 
    shiny::uiOutput(NS(id, "file_next"))
  )
}

# ---Select_columns_UI---

selectUI <- function(id){
  
  
}