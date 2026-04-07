# module of HLA I part of the submotif app

#library(Cairo) # removing this does not brake the app
#library(plotrix) # removing this does not break the app
#library(motifStack)
loadNamespace("motifStack")

submotifUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("HLA-I submotif collection"),

    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("type"), "Select HLA type:", choiceNames = c("HLA-I", "HLA-II"), choiceValues = c("I", "II")),
        uiOutput(ns("allele_ui")),
        uiOutput(ns("cluster_ui")),
        hr(),
        h4("General allele information"),
        plotOutput(ns("allele_motif")),
        textOutput(ns("allele_peptide_count")),
        plotOutput(ns("nmds_plot"), height = "400px")
      ),

      mainPanel(
        h4("Submotif and Entropy"),
        plotOutput(ns("submotif_plot")),

        # Download button for the submotif plot
        downloadButton(ns("download_submotif"), "Download Submotif Plot"),
        downloadButton(ns("download_submotif_data"), "Download Submotif Data (RDS)"),

        textOutput(ns("cluster_peptide_count")),

        # Új: peptidek helytakarékos megjelenítése egy accordionPanel-ben
        uiOutput(ns("peptide_ui")),  # AccordionPanel vagy toggle gomb a peptidekhez

        downloadButton(ns("download_peptide_list"), "Download Peptide List (TXT)"),

        plotOutput(ns("entropy_plot")),
        downloadButton(ns("download_entropy_plot"), "Download Entropy Plot (PNG)"),
        downloadButton(ns("download_entropy_data"), "Download Entropy Data (RDS)")
      )
    )
  )
}


########### SERVER #############


submotifServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    allele_data <- eventReactive(input$type, {
      req(input$type)
      if(input$type == "I"){
        hla_i_allele_data
      } else {
        hla_ii_allele_data
      }
      })
    submotifresults <- eventReactive(input$type,{
      req(input$type)
      if(input$type == "I"){
        hla_i_submotifresults
      } else {
        hla_ii_submotifresults
      }
    })


    # Get available HLA alleles for allele type
    available_alleles <- reactive({
      req(input$type)
      names(allele_data())
    })

    output$allele_ui <- renderUI({
      req(input$type)
      selectInput(ns("allele"), "Select an allele:", choices = names(submotifresults()))
    })

    # Dinamikusan elérhető klaszterek az adott allélhoz, peptid-szám szerint rendezetten
    available_clusters <- reactive({ # good practice: separate reactive and static code
      req(input$allele)
      getAvailableClusters(submotifresults(), allele_data(), input$allele)
    })

    output$cluster_ui <- renderUI({
      selectInput(ns("cluster"), "Select a cluster:", choices = available_clusters())
    })

    # Általános allél motívum
    output$allele_motif <- renderPlot({
      req(input$allele)
      motif <- allele_data()[[input$allele]]$motif
      motifStack::plot(motif)
    })

    # Peptid szám az allélhoz
    output$allele_peptide_count <- renderText({
      req(input$allele)
      n_peptides <- nrow(allele_data()[[input$allele]]$clusters)
      paste("Total number of peptides:", n_peptides)
    })

    # NMDS + klaszterek színezve
    output$nmds_plot <- renderPlot({
      req(input$allele, input$cluster)
      makeNmdsPlot(allele_data(), input$allele, input$cluster)
    })

    # Szubmotívum logó
    submotif <- reactive({
      req(input$cluster) #prevents plot generation upon start
      makeMotif(input$allele, input$cluster, submotifresults())
    })

    output$submotif_plot <- renderPlot({
      req(submotif()) #prevents plot generation upon start
      motifStack::plot(submotif())
    })

    # Letöltési handler a submotif plothoz (PNG)
    output$download_submotif <- downloadHandler(
      filename = function() { downloadFileName(input, "submotif_plot", ".png") },
      content = function(file) { makePng(file, input, submotif(), method = "motifStack") }
    )

    # Letöltési handler a submotif adat rds-ben
    output$download_submotif_data <- downloadHandler(
      filename = function() { downloadFileName(input, "submotif_plot", ".rds") },
      content = function(file) {  saveRDS(submotif(), file) }
    )

    # Klaszter peptidszám kiírás
    cluster_info <- reactive({
      getClusterInfo(input$allele, input$cluster,  allele_data())
    })

    peptides_in_cluster <- reactive({
      req(cluster_info, input$type, input$cluster)

      getClusterPeptides(cluster_info(), type = input$type, cluster = input$cluster)
    })

    output$cluster_peptide_count = renderText({
      req(peptides_in_cluster())
      n_peptides_in_cluster <- length(peptides_in_cluster())
      paste("Number of peptides in this cluster:", n_peptides_in_cluster)
    })

    # Klaszter peptidjeinek helytakarékos megjelenítése
    show_peptides <- reactiveVal(FALSE) # show peptides as a reactive

    observeEvent(input$show_peptides, { #change show petides, when the button is clicked
      show_peptides(!show_peptides())
    })

    output$peptide_ui <- renderUI({
      req(peptides_in_cluster())
      # Ha nincsenek peptidek, visszaadunk egy üzenetet
      if (length(peptides_in_cluster()) == 0) { return("No peptides in this cluster.") }

      tagList(
        actionButton(ns("show_peptides"), "Show Peptides"),
        if (show_peptides()) { # show only when showpeptides is TRUE
          div(
            style = "max-height: 300px; overflow-y: auto; columns: 5;",
            tags$ul(style = "margin: 0; padding-left: 20px;",
                    lapply(peptides_in_cluster(), tags$li) )
          )
        }
      )
    })

    # Peptid lista letöltése txt-ben
    output$download_peptide_list <- downloadHandler(
      filename = function() {  downloadFileName(input, "peptide_list_", ".txt")  },
      content = function(file) {  writeLines(peptides_in_cluster(), con = file) }
    )

    # Entrópia görbe
    entropy_values <- reactive({
      req(input$cluster)
      getEntropyValues(input$allele, input$cluster, submotifresults())
    })

    output$entropy_plot = renderPlot({
      req(input$cluster) #prevents plot render when input$cluster is missing upon startup

      plot(entropy_values(), type = "o", col = "darkgreen",
           xlab = "Position", ylab = "Entropy", pch = 16,
           main = paste("Entropy -", input$allele, "Cluster", input$cluster))
      abline(h = getRedLine(input$allele, input$cluster, submotifresults()), col = "red", lty = 2)
    })

    # Entrópia plot letöltése PNG-ben
    output$download_entropy_plot <- downloadHandler(
      filename = function() { downloadFileName(input, "entropy_plot_", ".png") },
      content = function(file) {
        png(file)
        plot(entropy_values(), type = "o", col = "darkgreen",
             xlab = "Position", ylab = "Entropy", pch = 16,
             main = paste("Entropy -", input$allele, "Cluster", input$cluster))
        abline(h = getRedLine(input$allele, input$cluster, submotifresults()), col = "red", lty = 2)
        dev.off()
      }
    )

    # Entrópia adat letöltése RDS-ben
    output$download_entropy_data <- downloadHandler(
      filename = function() { downloadFileName(input, "entropy_data_", ".rds") },
      content = function(file) { saveRDS(entropy_values(), file) }
    )
  })
}

