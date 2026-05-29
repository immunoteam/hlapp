
pkgload::load_all()

shiny::shinyApp(
  ui = HLApp_ui(),
  server = HLApp_server
)
