
# data table render options
DToptions <- list(dom = "Bfrtrip" ,
                  buttons = list(
                    list(extend = "excel", text = "Save as Excel", filename = "promiscuityApp_results"),
                    list(extend = "csv", text = "Save as CSV", filename = "promiscuityApp_results"),
                    list(extend = "copy", text = "Copy to Clipboard")
                  ), 
                  scrollX = T,
                  scrollY = 400, 
                  paging = F) 



promiscuityServer <- function(id, app_session, compare_values){
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
      
      #get the column name 
      col_name <- colnames(out_prom())[as.integer(click$col+1)]
      #get row name
      row_name <- out_prom()$ID[as.integer(click$row)]
      
      output$clickedval <- renderPrint(
        list(col_name, row_name)
        )
      
      updateActionButton(session, "funcdiv_button", disabled = !grepl("Func_div_", col_name))
      
      funcdiv_patient <- row_name
      funcdiv_locus <- gsub("Func_div_", "", col_name)
      funcdiv_alleles <- middle_table() |> 
        dplyr::filter(ID %in% funcdiv_patient, Locus %in% funcdiv_locus) |> 
        pull(HLA)
        
      compare_values$alleles <- funcdiv_alleles
      
      
    })
    
    observeEvent(input$funcdiv_button, {
      shiny::updateNavbarPage(app_session, inputId = "app_navbar", selected = "submotif_comp")
    })
    
    return(middle_table)
  })
}

