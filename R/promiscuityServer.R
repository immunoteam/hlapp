

promiscuityServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    input_df <- uploadServer("upload")
    
    selectColumnsServer("columns", input_df)
    
    
    
    main_cols <- reactive(colnames(input_df()))
    output$main_col_feedback <- renderPrint(main_cols())
    output$df_head <- DT::renderDT(head(input_df()))
  })
}
