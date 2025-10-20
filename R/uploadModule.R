# --- Upload UI ---

uploadModuleUI <- function(id){
  fluidRow(  
    shiny::fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")), 
    shiny::uiOutput(NS(id, "file_message")), 
    shiny::uiOutput(NS(id, "file_next"))
  )
}

# ---Upload server ---

uploadModuleServer <- function(id, main_session){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #read input table 
    in_df <- reactive({
      req(input$file)
      readInputFile(input$file)
    })
    
    #feedback on uploaded file
    output$file_message <- renderUI({
      if (!is.data.frame(in_df())) {
        tagList(
          tags$p("❌ Error reading file. Please check the format.", style = "color:red;")
        )
      } else {
        tagList(
          tags$p("✅ File read successfully!", style = "color:green;")
        )
      }
    })
    
    return(in_df)
  })
}