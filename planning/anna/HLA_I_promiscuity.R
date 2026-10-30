# required format: csv, seven columns named ID, A1, A2, B1, B3, C1, C2

library(shiny)
load("promiscuity_hlai_imm")

ui <- fluidPage(
  titlePanel("Promiscuity calculation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Select a file with list of the requested alleles",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ),
      radioButtons("separator", "Separator: ",
        choices = c(";", ",", ":"),
        selected = ";", inline = TRUE
      ),
      checkboxGroupInput("type", "Promiscuity to calculate:",
        choices = c(
          "HLA-A (individual alleles)",
          "HLA-B (individual alleles)",
          "HLA-C (individual alleles)",
          "HLA-A (average)", "HLA-B (average)",
          "HLA-C (average)", "all HLA-I (average)"
        ),
        selected = c(
          "HLA-A (average)", "HLA-B (average)",
          "HLA-C (average)"
        )
      ),
      downloadButton("download_selected", "Download selected"),
      downloadButton("download_all", "Download all")
    ),
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output) {
  calculate_promiscuity <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    data <- read.csv(file = input$file$datapath, header = TRUE, sep = input$separator)
    prom_df <- NULL
    prom_all <- NULL

    get_promiscuity <- function(columns) {
      next_sum <- rep(0, nrow(data))
      for (i in columns) {
        next_alleles <- data[, i]
        next_prom <- promiscuity[next_alleles]
        next_sum <- next_sum + next_prom
      }
      return(next_sum / length(columns))
    }

    A1 <- get_promiscuity("A1")
    A2 <- get_promiscuity("A2")
    A <- get_promiscuity(c("A1", "A2"))
    B1 <- get_promiscuity("B1")
    B2 <- get_promiscuity("B2")
    B <- get_promiscuity(c("B1", "B2"))
    C1 <- get_promiscuity("C1")
    C2 <- get_promiscuity("C2")
    C <- get_promiscuity(c("C1", "C2"))
    ALL <- get_promiscuity(c("A1", "A2", "B1", "B2", "C1", "C2"))


    if (("HLA-A (individual alleles)" %in% input$type)) {
      prom_df <- cbind(prom_df, A1, A2)
    }
    if (("HLA-A (average)" %in% input$type)) {
      prom_df <- cbind(prom_df, A)
    }
    if (("HLA-B (individual alleles)" %in% input$type)) {
      prom_df <- cbind(prom_df, B1, B2)
    }
    if (("HLA-B (average)" %in% input$type)) {
      prom_df <- cbind(prom_df, B)
    }
    if (("HLA-C (individual alleles)" %in% input$type)) {
      prom_df <- cbind(prom_df, C1, C2)
    }
    if (("HLA-C (average)" %in% input$type)) {
      prom_df <- cbind(prom_df, C)
    }
    if (("all HLA-I (average)" %in% input$type)) {
      prom_df <- cbind(prom_df, ALL)
    }
    rownames(prom_df) <- data$ID
    prom_df <- round(prom_df, 4)

    prom_all <- cbind(A1, A2, B1, B2, C1, C2, A, B, C, ALL)
    rownames(prom_all) <- data$ID

    return(list(prom_df, prom_all))
  })
  output$table <- DT::renderDataTable({
    req(calculate_promiscuity())
    prom_df <- calculate_promiscuity()
    prom_df[[1]]
  })
  output$download_selected <- downloadHandler(
    filename = function() {
      "promiscuity.csv"
    },
    content = function(file) {
      write.csv(calculate_promiscuity()[[1]], file, sep = input$separator)
    }
  )
  output$download_all <- downloadHandler(
    filename = function() {
      "promiscuity.csv"
    },
    content = function(file) {
      write.csv(calculate_promiscuity()[[2]], file, sep = input$separator)
    }
  )
}

shinyApp(ui = ui, server = server)
