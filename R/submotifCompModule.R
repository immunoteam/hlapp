
submotifCompUI <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(
    shiny::titlePanel("HLA submotif comparison"),
    verbatimTextOutput(ns("testing")),
    bslib::layout_columns(
      div(
        h5("Allele 1"),
        selectInput(ns("allele_1_sel"), label = "Choose allele", choices = allele_names),
        textOutput(ns("allele_1")),
        uiOutput(ns("allele_1_motif"))
      ),
      div(
        h5("Allele 2"),
        selectInput(ns("allele_2_sel"), label = "Choose allele", choices = allele_names),
        textOutput(ns("allele_2")),
        uiOutput(ns("allele_2_motif"))
      ),
      
    ),
    h4("Functional divergence of two alleles is: MISSING")
  )
  }

submotifCompServer <- function(id, compare_values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$testing <- renderPrint({
      reactiveValuesToList(compare_values)
    })
    
    alleles <- reactive( compare_values$alleles )
    
    observeEvent(alleles(), {
      req(alleles())
      updateSelectInput(session, "allele_1_sel", selected = alleles()[1])
      updateSelectInput(session, "allele_2_sel", selected = alleles()[2])
    })
    
    output$allele_1 <- renderText({
      req(input$allele_1_sel)
      paste("Motif of", input$allele_1_sel)
    })
    
    output$allele_1_motif <- shiny::renderUI({
      shiny::req(input$allele_1_sel)
      
      allele <- input$allele_1_sel
      
      file_name <- paste0( allele, ".svg"  )
      
      shiny::tags$img(
        src = file.path("img/motif", file_name),
        style = "width:100%; max-width:900px;"
      )
    })
    
    
    output$allele_2 <- renderText({
      req(input$allele_2_sel)
      paste("Motif of", input$allele_2_sel)
    })
    
    output$allele_2_motif <- shiny::renderUI({
      shiny::req(input$allele_2_sel)
      
      allele <- input$allele_2_sel
      
      file_name <- paste0( allele, ".svg"  )
      
      shiny::tags$img(
        src = file.path("img/motif", file_name),
        style = "width:100%; max-width:900px;"
      )
    })
  })
}

