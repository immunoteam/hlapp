

promiscuityServer <- function(id, login_status = NULL ) {
  moduleServer(id, function(input, output, session) {
    
    # place of loginserver
    output$login_feedback <- renderText(ifelse(login_status(), "Succesful login!", "Failed login. IF YOU SEE THIS MESSAGE CONTACT GERGŐ" ) )  
    
    output$promiscuity_interface <- renderUI(
      if(login_status()) {
        promiscuityUI2(id)
      } else {
        return(NULL)
      }
        
      
    )
    
    #options for datatable output
    DToptions <- list(dom = "Bfrtrip" ,
                      buttons = c("excel", "csv", "copy"), 
                      scrollX = T,
                      scrollY = 400, 
                      paging = F) 
    
    #change input, if patter recognotion is manual
    observeEvent(input$is_manual, {
      updateTabsetPanel(inputId = "input_switcher", selected = ifelse(input$is_manual, "manual", "automatic"))
    })
    
    # get the input data into a data table, 
    in_df <- reactive({
      req(input$file)
      readInputFile(input$file)
    })
    
    observeEvent(in_df(), {
      updateSelectInput(session, "idvar", choices = colnames(in_df() ) )
    })
    observeEvent(in_df(), {
      updateCheckboxGroupInput(session, "hlavar", choices = colnames(in_df() ) )
    })
    
    # create the two output tables
    middle_table <- eventReactive( input$submit, {
      formatInputTable(in_df(), manual = input$is_manual, id.col = input$idvar,  hla.cols = input$hlavar , flag.forbidden = T)  |>
        joinGenotypePromiscuity(promiscuity) |>
        dplyr::mutate_if(is.numeric, signif, 4)
    })
    
    out_prom <- eventReactive(input$submit, {
      hlaFromMiddleTable(middle_table(), types = input$type)
    })
    
    # output respones for uploaded file
    out_text <- reactive(ifelse(is.null(input$file), 
                                "Please upload a .xlsx, .tsv or .csv file",
                                "File received")
                         )
    # Render outputs
    output$resptext <- renderText( paste(names(in_df() ), collapse = ", " ) )
    
    #currently not in use
    output$input_head <- DT::renderDataTable(head(in_df(), n = 3),
                                             options = list(scrollX = T, paging = F, scrollY = 200, searching =F),
                                             rownames =F
                                             )
    
    output$middle_table <- DT::renderDataTable({
      req(middle_table())
      DT::datatable(middle_table(), 
                    rownames = FALSE,
                    extensions = "Buttons", 
                    options = DToptions)
    })
    
    output$prom_table <- DT::renderDataTable({
     # req(out_prom())
      DT::datatable(out_prom()[[1]], 
                    rownames = TRUE,
                    extensions = "Buttons", 
                    options = DToptions)
    })
    
    output$download_all <- downloadHandler(
      filename = "promiscuity.csv",
      content = function(file){
        utils::write.csv(out_prom()[[2]], file, row.names = T, quote = T)
      }
    )
  })
}
