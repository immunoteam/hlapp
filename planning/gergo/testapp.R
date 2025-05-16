library(shiny)

shinyApp(ui = fluidPage(promiscuityUI("test")),
         server = function(input, output, session) {
           promiscuityServer("test")
           }
)
