

histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
    #addon
    output$table <- renderTable({
      read.csv(input$file$datapath)
    })
    
    output$prom <- renderTable({
      readRDS("data/HLA_II_promiscuity.rds")
    })
  })
}
