

promiscuityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    require(dplyr)
    
    #loaddata
    #file <- file.path("data/HLA_II_promiscuity.rds")
    #promiscuity <- readRDS(file)
    
    #options for datatable output
    DToptions <- list(dom = "Bfrtrip" ,
                      buttons = c("excel", "csv", "copy"), 
                      scrollX = T ) 
    
    # get the input data into a data table, 
    in_df <- eventReactive(input$submit, {
      
      readInputFile(input$file)
    })
    
    
    # create the two output tables
    out_joined <- eventReactive( input$submit, {
      formatInputTable(in_df()) |>
        joinGenotypePromiscuity(promiscuity) |>
        mutate_if(is.numeric, signif, 4)
    })
    

    
    out_prom <- eventReactive( input$submit, {
      calcPromiscuity(out_joined() ) |>
        mutate_if(is.numeric, signif, 3)
    })
    
    # output respones for uploaded file
    out_text <- reactive(ifelse(is.null(input$file), 
                                "Please upload a .xlsx, .tsv or .csv file",
                                "File received")
                         )
    # Rneder outputs
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
    
  })
}
