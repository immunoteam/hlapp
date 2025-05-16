# testing the shiny applications. 

#' To Do : functions to translate multilpe formats of input. 
#'  file- extensions: tsv, csv, excel


# output goal DT::renderDatatable - downladable data table. - rows are patient IDs, columns are allele promiscuities
# input goal 

library(shiny)
library(dplyr)
library(tidyr)

#source functions
source(file.path("scr", "app_functions.R") )

#load data
file <- file.path("data/HLA_II_promiscuity.rds")
promiscuity <- readRDS(file)


### UI
ui <- fluidPage(
  fluidRow(
    fileInput(inputId = "file", label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")),
    textOutput("resptext") ,
    actionButton("submit",  "Submit file")
  ),
  fluidRow(
    column(6, h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput("joined_table") ),
    column(6, h4("Patient promiscuity:"), DT::dataTableOutput("prom_table") )
  )
)



### SERVER
server <- function(input, output, session){
  
  DToptions <- list(dom = "Bfrtrip" ,
                    buttons = c("excel", "csv", "copy"), 
                    scrollX = T ) 
  # Define outputs - outside of render functions - but inside the server function, ar reactive() objects
    # Text response  
  out_text <- reactive(ifelse(is.null(input$file), 
                          "Please upload a .tsv or .csv file",
                          "File received")
                       )
  
  #  file <- eventReactive(input$submit, {  input$file$datapath })
  in_df <- eventReactive(input$submit, {
    
    ext <- tools::file_ext(input$file$name)
    switch (ext,
      tsv = read.delim(input$file$datapath ),
      csv = read.csv(input$file$datapath), 
      xlsx = as.data.frame(readxl::read_excel(input$file$datapath))
    )
    
    
    })
  
  out_joined <- eventReactive( input$submit, {
    formatCohortTable(in_df(), select.cols = c("ID", "HLA_II") ) |>
                           joinGenotypePromiscuity(promiscuity, hla.col = "HLA_II") |>
                           mutate_if(is.numeric, signif, 4)
  })
  
  out_prom <- eventReactive( input$submit, {
    calcGenotypePromiscuity(out_joined() ) |>
                        mutate_if(is.numeric, signif, 3)
  })
                      

  # Render outputs
  output$resptext <- renderText( out_text() )
  
  output$prom_table <- DT::renderDataTable({
      DT::datatable(out_prom(), 
                    rownames = FALSE,
                    extensions = "Buttons", 
                    options = DToptions)
    })
  
  output$joined_table <- DT::renderDataTable({
      DT::datatable(out_joined(), 
                    rownames = FALSE,
                    extensions = "Buttons", 
                    options = DToptions)
    })
}

shinyApp(ui = ui, server = server)



#'  
#'  tips: 
#'  
#'  validate() for checking certain parameters of input
#'  values, fuctions can be defined outside of UI, just like base R
#'  observeEvent / EventReactive - buttons and action links
#'  
#'  do as little computation in render functions as possible ! 
#'  {} in render is only necessary if multiple lines are used

