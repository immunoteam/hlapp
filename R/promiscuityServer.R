
promiscuityServer <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    
    input_df <- uploadServer("upload", main_session = session)
    
    middle_table <- selectColumnsServer("columns", input_df, main_session = session)
    
    out_prom <- resultsServer("results", middle_table, main_session = session)
    
    # display results
    output$result_df <- DT::renderDT(out_prom(),
                                     rownames = FALSE,
                                     extensions = "Buttons", 
                                     options = DToptions, 
                                     selection = list(mode = 'single', target = 'cell'))
    
    
    
    observeEvent(input$result_df_cell_clicked,{
      click <- input$result_df_cell_clicked
      
      output$clickedval <- renderPrint(
        list(col_name, row_name)
        )
      
      #get the column name 
      col_name <- colnames(out_prom())[as.integer(click$col+1)]
      #get row name
      row_name <- out_prom()$ID[as.integer(click$row)]
      
      grepl("Functional", col_name)
        updateActionButton(session, "funcdiv_button", disabled = !grepl("_A", col_name))
      
      
    })
    
    return(middle_table)
  })
}

# data table render options
DToptions <- list(dom = "Bfrtrip" ,
                  buttons = c("excel", "csv", "copy"), 
                  scrollX = T,
                  scrollY = 400, 
                  paging = F) 
