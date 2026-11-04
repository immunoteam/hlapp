# module of HLA I part of the submotif app


submotifUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::titlePanel("HLA submotifs"),
    p("Here you can select HLA alleles, to explore their binding motifs and peptide submotifs. [MORE EXPLANTION CAN BE ADDED]"),

    # CSS styling for all download buttons
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .download-section {
          margin-top: 25px;
          padding-top: 15px;
          border-top: 1px solid #ddd;
        }

        .download-section h4 {
          margin-bottom: 15px;
        }

        .download-row {
          display: flex;
          align-items: center;
          gap: 12px;
          margin-bottom: 12px;
        }

        .download-section .btn-primary,
        .download-section .btn-default {
          background-color: #007bff !important;
          border-color: #007bff !important;
          color: white !important;
          min-width: 230px;
          text-align: center;
        }

        .download-section .btn:hover {
          background-color: #0056b3 !important;
          border-color: #0056b3 !important;
        }

        .download-description {
          font-size: 13px;
          color: #555;
        }
      "))
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          ns("type"),
          "Select HLA class:",
          choiceNames = c("HLA-I", "HLA-II"),
          choiceValues = c("HLA_I", "HLA_II")
        ),
        shiny::uiOutput(ns("allele_ui")),
        shiny::uiOutput(ns("cluster_ui")),
        shiny::hr(),
        shiny::h4("General allele information"),
        shiny::uiOutput(ns("allele_motif")),
        shiny::textOutput(ns("allele_peptide_count")),
        shiny::plotOutput(ns("nmds_plot"), height = "400px")
      ),
      shiny::mainPanel(
        shiny::h4("Submotif of the selected peptid cluster"),

        # Submotif plot
        shiny::uiOutput(ns("submotif_plot")),
        shiny::textOutput(ns("cluster_peptide_count")),

        # Peptide list interface
        shiny::uiOutput(ns("peptide_ui")),

        # Entropy plot
        shiny::h4("Entropy of the AA positions of the submotif"),
        shiny::plotOutput(ns("entropy_plot")),

        # All download buttons grouped at the bottom
        shiny::div(
          class = "download-section",
          shiny::h4("Download data"),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(
              ns("download_submotif"),
              "Download Submotif Plot",
              class = "btn-primary"
            ),
            shiny::span(
              class = "download-description",
              "Download the submotif plot as an image."
            )
          ),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(
              ns("download_submotif_data"),
              "Download Submotif Data (TXT)",
              class = "btn-primary"
            ),
            shiny::span(
              class = "download-description",
              "Download the underlying submotif data in TXT format."
            )
          ),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(
              ns("download_peptide_list"),
              "Download Peptide List (TXT)",
              class = "btn-primary"
            ),
            shiny::span(
              class = "download-description",
              "Download the peptide list as a plain-text file."
            )
          ),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(
              ns("download_entropy_plot"),
              "Download Entropy Plot (PNG)",
              class = "btn-primary"
            ),
            shiny::span(
              class = "download-description",
              "Download the entropy plot as a PNG image."
            )
          ),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(
              ns("download_entropy_data"),
              "Download Entropy Data (TXT)",
              class = "btn-primary"
            ),
            shiny::span(
              class = "download-description",
              "Download the underlying entropy data in TXT format."
            )
          )
        )
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
      shiny::selectizeInput(ns("allele"), "Select allele form list, or start typing:", choices = available_alleles())
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

    cluster_data <- shiny::eventReactive(list(input$type, input$allele, input$cluster), {
      shiny::req(input$type, input$allele, input$cluster)

      read_cluster(input$type, input$allele, input$cluster)
    })

    # Általános allél motívum
    output$allele_motif <- shiny::renderUI({
      shiny::req(input$allele)

      file_name <- paste0(input$allele, ".svg")

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

      file_name <- paste0(input$allele, "_cluster_", input$cluster, ".svg")

      shiny::tags$img(
        src = file.path("img/submotif", file_name),
        style = "width:100%; max-width:900px;"
      )
    })

    # Download handler for submotif plot (SVG)
    output$download_submotif <- shiny::downloadHandler(
      filename = function() {
        downloadFileName(input, "submotif_plot", ".svg")
      },
      content = function(file) {
        svg_path <- file.path("www/img/submotif", paste0(input$allele, "_cluster_", input$cluster, ".svg"))
        file.copy(svg_path, file)
      }
    )

    # Download handler for submotif cluster data (RDS)
    output$download_submotif_data <- shiny::downloadHandler(
      filename = function() {
        paste0(input$allele, "_cluster_", input$cluster, "_motif.txt")
      },
      content = function(file) {
        # txt_path <- file.path("data", "cluster_motif", paste0(input$allele, "_cluster_", input$cluster, "_motif.txt"))
        motif_txt <- cluster_data()$entropy$freq
        colnames(motif_txt) <- paste0("pos_", seq_len(ncol(motif_txt)))
        write.table(motif_txt, file, row.names = TRUE, quote = FALSE, sep = "\t")
      }
    )

    # Peptidek száma
    output$cluster_peptide_count <- shiny::renderText({
      shiny::req(cluster_data())
      n_peptides_in_cluster <- length(cluster_data()$peptides)
      paste("Number of peptides in this cluster:", n_peptides_in_cluster)
    })

    # Klaszter peptidjeinek helytakarékos megjelenítése
    show_peptides <- shiny::reactiveVal(FALSE) # show peptides as a reactive
    shiny::observeEvent(input$show_peptides, { # change show petides, when the button is clicked
      show_peptides(!show_peptides())
    })

    output$peptide_ui <- shiny::renderUI({
      shiny::req(cluster_data())
      # Ha nincsenek peptidek, visszaadunk egy üzenetet
      if (length(cluster_data()$peptides) == 0) {
        return("No peptides in this cluster.")
      }

      shiny::tagList(
        if (show_peptides()) {
          shiny::actionButton(ns("show_peptides"), "Hide peptides")
        } else {
          shiny::actionButton(ns("show_peptides"), "Show peptides")
        },
        if (show_peptides()) { # show only when showpeptides is TRUE
          shiny::div(
            style = "max-height: 300px; overflow-y: auto; columns: 5;",
            shiny::tags$ul(
              style = "margin: 0; padding-left: 20px;",
              lapply(cluster_data()$peptides, shiny::tags$li)
            )
          )
        }
      )
    })

    # Peptid lista letöltése txt-ben
    output$download_peptide_list <- shiny::downloadHandler(
      filename = function() {
        downloadFileName(input, "peptide_list_", ".txt")
      },
      content = function(file) {
        writeLines(cluster_data()$peptides, con = file)
      }
    )

    # Entrópia görbe
    output$entropy_plot <- shiny::renderPlot({
      shiny::req(cluster_data()) # prevents plot render when input$cluster is missing upon startup

      makeEntropyPlot(cluster_data()$entropy$H)
    })

    # Entrópia plot letöltése PNG-ben
    output$download_entropy_plot <- shiny::downloadHandler(
      filename = function() {
        downloadFileName(input, "entropy_plot_", ".png")
      },
      content = function(file) {
        png(file)
        makeEntropyPlot(cluster_data()$entropy$H)
        dev.off()
      }
    )

    # Entrópia adat letöltése TXT-ben
    output$download_entropy_data <- shiny::downloadHandler(
      filename = function() {
        downloadFileName(input, "entropy_data", ".txt")
      },
      content = function(file) {
        entropy_txt <- data.frame(entropy = cluster_data()$entropy$H)
        write.table(entropy_txt, file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    )

    return(session)
  })
}
