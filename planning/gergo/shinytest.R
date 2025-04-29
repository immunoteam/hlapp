# testing the shiny applications. 


# output goal DT::renderDatatable - downladable data table. - rows are patient IDs, columns are allele promiscuities
# input goal 

library(shiny)
library(dplyr)
library(tidyr)
source("R/01_promiscuity_calculation.R")
source("R/shiny_backend.R")


### UI
ui <- fluidPage(
  fileInput(inputId = "File", label = "upload HLA genotype file"),
  submitButton(),
  textOutput(outputId = "resptext"),
  DT::dataTableOutput(outputId = "DT")
)



### SERVER
server <- function(input, output, session){

  output$resptext <- renderText({
    if (is.null(input$File)){
      print("Please upload a .tsv file")
    }
  })
  
  output$DT <- DT::renderDataTable({
    
    if(!is.null(input$File)) {
      
      file<- input$File
      input.df <- read.delim(file$datapath, sep = "\t")
      
      out <- formatCohortTable(input.df, select.cols = c("ID", "HLA_II")) |>
        calcGenotypePromiscuity(promiscuity, hla.col = "HLA_II") |> 
        mutate_if(is.numeric, signif, 3)  # round output to 3 digits
        
      DT::datatable(out, extensions = "Buttons", 
                       options = list(dom = "Bfrtrip" ,
                                      buttons = c("excel", "csv"), 
                                      scrollX = T 
                                      )  
                       )
    }
  })
}

shinyApp(ui = ui, server = server)


#' develop backend 
#' create stamdardised rormat after inout
#' functions to calculate all available promiscuities
#' return outout as data tables
