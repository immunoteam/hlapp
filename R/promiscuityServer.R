
promiscuityServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    input_df <- uploadServer("upload", main_session = session)
    
    middle_table <- selectColumnsServer("columns", input_df, main_session = session)
    
    out_prom <- resultsServer("results", middle_table, main_session = session)
    
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