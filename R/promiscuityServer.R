
promiscuityServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    input_df <- uploadServer("upload")
    
    observeEvent(input$upload_next, {
      updateTabsetPanel(session, "main_tabs", selected = "2. Select input columns")
    })
    
    middle_table <- selectColumnsServer("columns", input_df)
    
    out_prom <- resultsServer("results", middle_table)
    
    #main_cols <- reactive(colnames(input_df()))
    #output$main_col_feedback <- renderPrint(main_cols())
    output$result_df <- DT::renderDT(head(out_prom()[[1]]),
                                   rownames = FALSE,
                                   extensions = "Buttons", 
                                   options = DToptions)
  })
}

# data table render options
DToptions <- list(dom = "Bfrtrip" ,
                  buttons = c("excel", "csv", "copy"), 
                  scrollX = T,
                  scrollY = 400, 
                  paging = F) 