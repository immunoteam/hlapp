
library(shiny)
library(dplyr)
library(tidyr)

shinyApp(ui = fluidPage(promiscuityUI("test")),
         server = function(input, output, session) {
           promiscuityServer("test")
           }
)
