# testing the shiny applications. 


# output goal DT::renderDatatable - downladable data table. - rows are patient IDs, columns are allele promiscuities
# input goal 

library(shiny)
library(dplyr)
library(tidyr)

dir <- file.path("planning", "gergo")

source(file.path(dir, "scr", "01_promiscuity_calculation.R") )
source(file.path(dir, "scr", "shiny_backend.R") )


### UI
ui <- fluidPage(
  fileInput(inputId = "file", label = "upload HLA genotype file"),
  submitButton(),
  textOutput(outputId = "resptext"),
  DT::dataTableOutput(outputId = "DT")
)



### SERVER
server <- function(input, output, session){
  
  # Define outputs - outside of render functions - but inside the server function, ar reactive() objects
    # Text response  
  out_text <- reactive(ifelse(is.null(input$file), 
                          "Please upload a .tsv file",
                          "File received"
                          )
                       )
    # Output data table 
 # is_input_null <- reactive( is.null(input$file ))
  #input_file <- reactive(input$file)
  #input_df <- reactive( read.delim(input$file$datapath, sep = "\t" ) )
  #out_df <- reactive( input$file )
  file <- reactive(input$File$datapath)
  in_df <- reactive(read.delim(file() ) )
  out_df <- reactive( formatCohortTable(in_df(), select.cols = c("ID", "HLA_II") ) |>
                        calcGenotypePromiscuity(promiscuity, hla.col = "HLA_II") |> 
                        mutate_if(is.numeric, signif, 3)
                      )

  # Render outputs
  output$resptext <- renderText( out_text() )
  
  output$DT <- DT::renderDataTable({
        
    #out_df <- read.delim(file = file())
      DT::datatable(out_df(), extensions = "Buttons", 
                       options = list(dom = "Bfrtrip" ,
                                      buttons = c("excel", "csv", "copy"), 
                                      scrollX = T )  
                       )
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

