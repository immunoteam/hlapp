# module of HLA I part of the submotif app


submotifUI <- function(id) {
  ns <- shiny::NS(id)
   shiny::tabsetPanel(
     
    shiny::tabPanel( "Explore", value = "explore",
      shiny::fluidPage(
        shiny::titlePanel("HLA-I submotif collection"),
    
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::radioButtons(ns("type"), "Select HLA devtype:", choiceNames = c("HLA-I", "HLA-II"), choiceValues = c("HLA_I", "HLA_II")),
            shiny::uiOutput(ns("allele_ui")),
            shiny::uiOutput(ns("cluster_ui")),
            shiny::hr(),
            shiny::h4("General allele information"),
            shiny::uiOutput(ns("allele_motif")),
            shiny::textOutput(ns("allele_peptide_count")),
            shiny::plotOutput(ns("nmds_plot"), height = "400px")
          ),
    
          shiny::mainPanel(
            shiny::h4("Submotif and Entropy"),
            shiny::uiOutput(ns("submotif_plot")),
    
            # Download button for the submotif plot
            shiny::downloadButton(ns("download_submotif"), "Download Submotif Plot"),
            shiny::downloadButton(ns("download_submotif_data"), "Download Submotif Data (RDS)"),
    
            shiny::textOutput(ns("cluster_peptide_count")),
    
            # Új: peptidek helytakarékos megjelenítése egy accordionPanel-ben
            shiny::uiOutput(ns("peptide_ui")),  # AccordionPanel vagy toggle gomb a peptidekhez
    
            shiny::downloadButton(ns("download_peptide_list"), "Download Peptide List (TXT)"),
    
            shiny::plotOutput(ns("entropy_plot")),
            shiny::downloadButton(ns("download_entropy_plot"), "Download Entropy Plot (PNG)"),
            shiny::downloadButton(ns("download_entropy_data"), "Download Entropy Data (RDS)")
          )
        )
      )
    ), 
    shiny::tabPanel( "Compare", value = "compare",
            shiny::fluidPage(
              shiny::titlePanel("Compare Motifs")
            )
                     )
  )
}


########### SERVER #############


submotifServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    # Get available HLA alleles for allele type
    available_alleles <- shiny::eventReactive(input$type, {
      shiny::req(input$type)
      allele_names[[input$type]]
    })

    output$allele_ui <- shiny::renderUI({
      shiny::req(input$type)
      shiny::selectInput(ns("allele"), "Select an allele:", choices = available_alleles() )
    })
    
    allele_data <- shiny::eventReactive(input$allele, {
      shiny::req(input$allele)
      read_allele(input$allele)
      
    })

    # Dinamikusan elérhető klaszterek az adott allélhoz, peptid-szám szerint rendezetten
    available_clusters <- shiny::reactive({ # good practice: separate reactive and static code
      shiny::req(input$allele)
      allele_data()$clusters$Cluster
    })

    output$cluster_ui <- shiny::renderUI({
      shiny::selectInput(ns("cluster"), "Select a cluster:", choices = available_clusters())
    })
    
    cluster_data <- shiny::eventReactive(list(input$type, input$allele, input$cluster),{
        shiny::req(input$type, input$allele, input$cluster)
      
        read_cluster(input$type, input$allele, input$cluster)
      }
    )

    # Általános allél motívum
    output$allele_motif <- shiny::renderUI({
      shiny::req(input$allele)
      
      file_name <- paste0( input$allele, ".svg"  )
      
      shiny::tags$img(
        src = file.path("img/motif", file_name),
        style = "width:100%; max-width:900px;"
      )
    })
    

    # Peptid szám az allélhoz
    output$allele_peptide_count <- shiny::renderText({
      shiny::req(input$allele)
      n_peptides <- sum(allele_data()$clusters$Peptide_n)
      paste("Total number of peptides:", n_peptides)
    })

    # NMDS + klaszterek színezve
    output$nmds_plot <- shiny::renderPlot({
      shiny::req(input$allele, input$cluster)
      makeNmdsPlot(allele_data(), input$allele, input$cluster)
    })
    
    # Submotif plot
    output$submotif_plot <- shiny::renderUI({
      shiny::req(input$allele, input$cluster)
      
      file_name <- paste0(input$allele, "_cluster", input$cluster, ".svg"  )
      
      shiny::tags$img(
        src = file.path("img/submotif", file_name),
        style = "width:100%; max-width:900px;"
      )
    })

    # Letöltési handler a submotif plothoz (PNG)
    #output$download_submotif <- shiny::downloadHandler(
      #filename = function() { downloadFileName(input, "submotif_plot", ".png") },
      #content = function(file) { makePng(file, input, cluster_data()$motif, method = "motifStack") }
    #)

    # Letöltési handler a submotif adat rds-ben
    #output$download_submotif_data <- shiny::downloadHandler(
      #filename = function() { downloadFileName(input, "submotif_plot", ".rds") },
      #content = function(file) {  saveRDS(cluster_data()$motif, file) }
    #)

    #Peptidek száma
    output$cluster_peptide_count = shiny::renderText({
      shiny::req(cluster_data())
      n_peptides_in_cluster <- length(cluster_data()$peptides)
      paste("Number of peptides in this cluster:", n_peptides_in_cluster)
    })

    # Klaszter peptidjeinek helytakarékos megjelenítése
    show_peptides <- shiny::reactiveVal(FALSE) # show peptides as a reactive
    shiny::observeEvent(input$show_peptides, { #change show petides, when the button is clicked
      show_peptides(!show_peptides())
    })

    output$peptide_ui <- shiny::renderUI({
      shiny::req(cluster_data())
      # Ha nincsenek peptidek, visszaadunk egy üzenetet
      if (length(cluster_data()$peptides) == 0) { return("No peptides in this cluster.") }

      shiny::tagList(
        shiny::actionButton(ns("show_peptides"), "Show Peptides"),
        if (show_peptides()) { # show only when showpeptides is TRUE
          shiny::div(
            style = "max-height: 300px; overflow-y: auto; columns: 5;",
            shiny::tags$ul(style = "margin: 0; padding-left: 20px;",
                    lapply(cluster_data()$peptides, shiny::tags$li) )
          )
        }
      )
    })

    # Peptid lista letöltése txt-ben
    output$download_peptide_list <- shiny::downloadHandler(
      filename = function() {  downloadFileName(input, "peptide_list_", ".txt")  },
      content = function(file) {  writeLines(cluster_data()$peptides, con = file) }
    )

    # Entrópia görbe
    output$entropy_plot = shiny::renderPlot({
      shiny::req(input$cluster) #prevents plot render when input$cluster is missing upon startup

      plot(cluster_data()$entropy$H, type = "o", col = "darkgreen",
           xlab = "Position", ylab = "Entropy", pch = 16,
           main = paste("Entropy -", input$allele, "Cluster", input$cluster))
      abline(h = cluster_data()$red_line, col = "red", lty = 2)
    })

    # Entrópia plot letöltése PNG-ben
    output$download_entropy_plot <- shiny::downloadHandler(
      filename = function() { downloadFileName(input, "entropy_plot_", ".png") },
      content = function(file) {
        png(file)
        plot(cluster_data()$entropy$H, type = "o", col = "darkgreen",
             xlab = "Position", ylab = "Entropy", pch = 16,
             main = paste("Entropy -", input$allele, "Cluster", input$cluster))
        abline(h = cluster_data()$red_line, col = "red", lty = 2)
        dev.off()
      }
    )

    # Entrópia adat letöltése RDS-ben
    output$download_entropy_data <- shiny::downloadHandler(
      filename = function() { downloadFileName(input, "entropy_data_", ".rds") },
      content = function(file) { saveRDS(cluster_data()$entropy$H, file) }
    )
    
    
    #observe
  })
}
