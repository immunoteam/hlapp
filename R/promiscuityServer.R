
promiscuityServer <- function(id, app_session, subm_session){
  shiny::moduleServer(id, function(input, output, session) {
    
    input_df <- uploadServer("upload", prom_session = session)
    
    middle_table <- selectColumnsServer("columns", input_df, prom_session = session)
    
    out_prom <- resultsServer("results", middle_table, prom_session = session)
    
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
      
      updateActionButton(session, "funcdiv_button", disabled = !grepl("Func_div_", col_name))
    })
    
    observeEvent(input$funcdiv_button, {
      shiny::updateNavbarPage(app_session, inputId = "app_navbar", selected = "submotif")
      bslib::nav_select("submotif_tab", selected = "compare", session = subm_session)
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
