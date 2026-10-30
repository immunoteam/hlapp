submotifCompUI <- function(id) {
  ns <- shiny::NS(id)

  fluidPage(
    shiny::titlePanel("HLA submotif comparison"),
    p("Here the binding motifs of two alleles are shown beside each other for comparison."),
    p("Below the plots the binding motifs are shown in a text format (only available for HLA-I alleles.)"),
    # verbatimTextOutput(ns("testing")),
    bslib::layout_columns(
      div(
        h5("Allele 1"),
        selectInput(ns("allele_1_sel"), label = "Choose allele", choices = allele_names),
        textOutput(ns("allele_1")),
        uiOutput(ns("allele_1_motif")),
        verbatimTextOutput(ns("allele_1_submotifs"))
      ),
      div(
        h5("Allele 2"),
        selectInput(ns("allele_2_sel"), label = "Choose allele", choices = allele_names),
        textOutput(ns("allele_2")),
        uiOutput(ns("allele_2_motif")),
        verbatimTextOutput(ns("allele_2_submotifs"))
      )
    ),
    p("Functional divergence between the two alleles is: (Only available for HLA-I alleles, within a HLA locus)"),
    verbatimTextOutput(ns("funcdiv_value"))
  )
}

submotifCompServer <- function(id, compare_values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$testing <- renderPrint({ reactiveValuesToList(compare_values)  })
    ## receive input
    # Track whether the inputs have been initialized
    initialized <- reactiveVal(FALSE)

    # Synchronize when inputs become available or external values change
    observe({
      req(length(compare_values$alleles) == 2)
      req(!is.null(input$allele_1_sel), !is.null(input$allele_2_sel))

      if (!initialized()) {
        updateSelectInput(session, "allele_1_sel", selected = compare_values$alleles[1])
        updateSelectInput(session, "allele_2_sel", selected = compare_values$alleles[2])
        initialized(TRUE)
      }
    })

    # Synchronize only when external values change
    observeEvent(compare_values$alleles,
      {
        req(initialized())
        req(length(compare_values$alleles) == 2)

        updateSelectInput(session, "allele_1_sel", selected = compare_values$alleles[1])
        updateSelectInput(session, "allele_2_sel", selected = compare_values$alleles[2])
      },
      ignoreInit = TRUE
    )


    ### Output
    output$allele_1 <- renderText({
      req(input$allele_1_sel)
      paste("Motif of", input$allele_1_sel)
    })

    output$allele_1_motif <- shiny::renderUI({
      shiny::req(input$allele_1_sel)
      getCompPlot(input$allele_1_sel)
    })

    output$allele_1_submotifs <- renderPrint({
      req(input$allele_1_sel)
      getSubmotifText(input$allele_1_sel)
    })

    output$allele_2 <- renderText({
      req(input$allele_2_sel)
      paste("Motif of", input$allele_2_sel)
    })

    output$allele_2_motif <- shiny::renderUI({
      shiny::req(input$allele_2_sel)
      getCompPlot(input$allele_2_sel)
    })

    output$allele_2_submotifs <- renderPrint({
      req(input$allele_2_sel)
      getSubmotifText(input$allele_2_sel)
    })

    output$funcdiv_value <- renderPrint({
      req(input$allele_1_sel, input$allele_2_sel)

      sorted <- sort(c(input$allele_1_sel, input$allele_2_sel))

      funcdiv_val <- func_div_data |>
        filter(Allele1 == sorted[1], Allele2 == sorted[2]) |>
        pull(FuncDiv)

      cat(ifelse(length(funcdiv_val) == 1, funcdiv_val, NA))
    })
  })
}
