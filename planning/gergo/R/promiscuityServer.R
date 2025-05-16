

promiscuityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    require(dplyr)
    
    #options for datatable output
    file <- file.path("data/HLA_II_promiscuity.rds")
    promiscuity <- readRDS(file)
    
    
    DToptions <- list(dom = "Bfrtrip" ,
                      buttons = c("excel", "csv", "copy"), 
                      scrollX = T ) 
    
    # get the input data into a data table, 
    in_df <- eventReactive(input$submit, {
      
      ext <- tools::file_ext(input$file$name)
      switch (ext,
              tsv = read.delim(input$file$datapath ),
              csv = read.csv(input$file$datapath), 
              xlsx = as.data.frame(readxl::read_excel(input$file$datapath))
      )
    })
    
    
    # create the two output tables
    out_joined <- eventReactive( input$submit, {
      formatCohortTable(in_df(), select.cols = c("ID", "HLA_II") ) |>
        joinGenotypePromiscuity(promiscuity, hla.col = "HLA_II") |>
        mutate_if(is.numeric, signif, 4)
    })
    

    
    out_prom <- eventReactive( input$submit, {
      calcGenotypePromiscuity(out_joined() ) |>
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
