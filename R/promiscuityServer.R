
promiscuityServer <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    
    input_df <- uploadServer("upload", main_session = session)
    
    middle_table <- selectColumnsServer("columns", input_df, main_session = session)
    
    out_prom <- resultsServer("results", middle_table, main_session = session)
    
    # display results
    output$result_df <- DT::renderDT(out_prom(),
                                     rownames = FALSE,
                                     extensions = "Buttons", 
                                     options = DToptions)
    
    return(middle_table)
  })
}

# data table render options
DToptions <- list(dom = "Bfrtrip" ,
                  buttons = c("excel", "csv", "copy"), 
                  scrollX = T,
                  scrollY = 400, 
                  paging = F) 
