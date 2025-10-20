# --- Upload UI ---

uploadModuleUI <- function(id){
  fluidRow(  
    shiny::fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")), 
    shiny::uiOutput(NS(id, "file_message")), 
    shiny::uiOutput(NS(id, "file_next"))
  )
}

# ---Upload server ---
# To do...