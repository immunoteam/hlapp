
# modul for selecting and calculating results

########### UI ################xxx

resultsUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    
    shiny::p("Select the HLA metrics that you want to be calculated. Results will be returned E.as "),
    shiny::uiOutput(ns("avail_feedback")),
    shiny::uiOutput(ns("warning_feedback")),
    shiny::uiOutput(ns("more_feedback")),
    bslib::layout_columns(
      shiny::div(
        shiny::p("Allele promiscuity"),
        shiny::checkboxInput(ns("res_ind_all"),  "Select / Deselect all", value = FALSE),
        shiny::checkboxGroupInput(ns("res_ind"), NULL, choices = character())
        ),
      shiny::div(
        shiny::p("Locus promiscuity"),
        shiny::checkboxInput(ns("res_avg_all"),  "Select / Deselect all", value = FALSE),
        shiny::checkboxGroupInput(ns("res_avg"), NULL, choices = character())  
      ),
      shiny::radioButtons(ns("res_homo"), "Homozygosity", choiceNames = c("yes", "no"), choiceValues = c(TRUE, FALSE), selected = FALSE), # get homozygosity
      shiny::checkboxGroupInput(ns("res_evdiv"), "HLA evolutionary divergence (HED)", choiceNames = c("HLA-I", "HLA-II"), choiceValues = c("I", "II")), # get evolutionary divergence
      shiny::checkboxGroupInput(ns("res_funcdiv"), "Functional divergence", choiceNames = c("HLA-A", "HLA-B", "HLA-C"), choiceValues = c("A", "B", "C")) # get evolutionary divergence
      
    ),
    bslib::layout_columns(
      shiny::actionButton(ns("result_back"), "Go back", disabled = FALSE),
      shiny::uiOutput(ns("results_next_ui")),
      col_widths = c(2, -6, 2, -2)
    )
   #, verbatimTextOutput(ns("test"))
  )
}

########## SERVER ################

###### Control input UI ######
resultsServer <- function(id, middle_table, prom_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Store all locus-related information in one reactiveValues object
    locus_data <- shiny::reactiveValues(
      avail_loci = character(0),
      avail_type = character(0),
      avail_all = character(0),
      funcdiv_loci = character(0),
      warn_loci = character(0),
      more_loci = character(0)
    )
    
    # Update all locus-related values whenever middle_table changes
    shiny::observeEvent(middle_table(), {
      
      # Available loci
      locus_data$avail_loci <- sort(unique(middle_table()[["Locus"]]))
      
      # Loci with only one allele
      locus_data$warn_loci <- checkFor2Alleles(middle_table())
      
      # Loci with more than two alleles
      locus_data$more_loci <- checkForMoreAlleles(middle_table())
      
      # Available locus types
      locus_data$avail_type <- dplyr::recode_values(
        locus_data$avail_loci,
        c("A", "B", "C") ~ "class-I",
        c("DP", "DQ", "DR") ~ "class-II"
      ) |>
        unique()
      
      # All available loci and locus types
      locus_data$avail_all <- c(
        locus_data$avail_loci,
        locus_data$avail_type
      )
      
      # Loci available for functional divergence
      locus_data$funcdiv_loci <- intersect(
        locus_data$avail_all,
        c("A", "B", "C", "class-I")
      )
      
    }, ignoreInit = FALSE)
    
    
    # Check whether any loci are available
    avail_check <- shiny::reactive(
      length(locus_data$avail_loci) > 0
    )
    
    # User feedback
    output$avail_feedback <- shiny::renderUI(
      giveFeedBack(
        avail_check(),
        paste(
          texts$results_avail_pass,
          paste(locus_data$avail_loci, collapse = ", ")
        ),
        texts$results_avail_fail
      )
    )
    
    output$warning_feedback <- shiny::renderUI(
      giveFeedBack(
        length(locus_data$warn_loci) == 0,
        "",
        paste(
          "Warning: The following HLA-loci has only one allele in the input:",
          paste(locus_data$warn_loci, collapse = ", "),
          "---Some results will not be calculated."
        )
      )
    )
    
    output$more_feedback <- shiny::renderUI(
      giveFeedBack(
        length(locus_data$more_loci) == 0,
        "",
        paste(
          "ERROR!: The following HLA-loci has more than 2 alleles in the input:",
          paste(locus_data$more_loci, collapse = ", "),
          "---Fix this to proceed!"
        )
      )
    )
    
    
    # Update available checkbox choices
    shiny::observeEvent(avail_check(), {
      
      if (avail_check()) {
        
        shiny::updateCheckboxGroupInput(
          session,
          "res_ind",
          choiceNames = paste("HLA-", locus_data$avail_loci),
          choiceValues = paste0(locus_data$avail_loci, "i")
        )
        
        shiny::updateCheckboxGroupInput(
          session,
          "res_avg",
          choiceNames = paste("HLA-", locus_data$avail_all),
          choiceValues = paste0(locus_data$avail_all, "a")
        )
        
        funcdiv_is_0 <- length(locus_data$funcdiv_loci) == 0
        
        shiny::updateCheckboxGroupInput(
          session,
          "res_funcdiv",
          choiceNames = if (funcdiv_is_0) {
            "Unavailable"
          } else {
            paste0("HLA-", locus_data$funcdiv_loci)
          },
          choiceValues = if (funcdiv_is_0) {
            "Unavailable"
          } else {
            locus_data$funcdiv_loci
          }
        )
        
        shiny::updateCheckboxGroupInput(
          session,
          "res_evdiv",
          choiceNames = paste0("HLA-", locus_data$avail_all),
          choiceValues = locus_data$avail_all
        )
      }
      
    }, ignoreInit = FALSE)
    
    
    # Select / deselect all individual promiscuity values
    shiny::observeEvent(input$res_ind_all, {
      
      ind_values <- paste0(locus_data$avail_loci, "i")
      
      shiny::updateCheckboxGroupInput(
        session,
        "res_ind",
        selected = if (input$res_ind_all) {
          ind_values
        } else {
          character(0)
        }
      )
      
    })
    
    
    # Select / deselect all average promiscuity values
    shiny::observeEvent(input$res_avg_all, {
      
      avg_values <- paste0(locus_data$avail_all, "a")
      
      shiny::updateCheckboxGroupInput(
        session,
        "res_avg",
        selected = if (input$res_avg_all) {
          avg_values
        } else {
          character(0)
        }
      )
      
    })
    
    
    # Calculate selected promiscuity values
    res_types <- shiny::reactive(
      c(input$res_ind, input$res_avg)
    )
    
    out_prom <- shiny::eventReactive(input$result_next, {
      hlaFromMiddleTable(
        middle_table(),
        res_types()
      )
    })
    
    
    # Calculate homozygosity
    out_homo <- shiny::eventReactive(input$result_next, {
      
      if (isTRUE(input$res_homo)) {
        detectHomozygotes(
          middle_table(),
          warn = locus_data$warn_loci
        ) |>
          as.data.frame()
      } else {
        makePlaceHolder(middle_table())
      }
      
    })
    
    
    # Calculate evolutionary divergence
    out_evdiv <- shiny::eventReactive(input$result_next, {
      
      if (length(input$res_evdiv) > 0) {
        evolutionaryDivergence(
          middle_table(),
          selected = input$res_evdiv,
          warn = locus_data$warn_loci
        ) |>
          as.data.frame()
      } else {
        makePlaceHolder(middle_table())
      }
      
    })
    
    
    # Calculate functional divergence
    out_funcdiv <- shiny::eventReactive(input$result_next, {
      
      if (length(input$res_funcdiv) > 0) {
        
        if ("Unavailable" %in% input$res_funcdiv) {
          makePlaceHolder(middle_table())
        } else {
          functionalDivergence(
            middle_table(),
            selected = input$res_funcdiv,
            warn = locus_data$warn_loci
          ) |>
            as.data.frame()
        }
        
      } else {
        makePlaceHolder(middle_table())
      }
      
    })
    
    
    # Navigation
    shiny::observeEvent(input$result_back, {
      shiny::updateTabsetPanel(
        prom_session,
        "prom_tabs",
        selected = "select"
      )
    })
    
    
    # Check whether results can be calculated
    res_check_ok <- shiny::reactive({
      
      any(
        length(res_types()) > 0,
        isTRUE(input$res_homo),
        length(input$res_evdiv) > 0,
        length(input$res_funcdiv) > 0
      ) && length(locus_data$more_loci) == 0
      
    })
    
    
    # Results button
    output$results_next_ui <- shiny::renderUI({
      
      if (res_check_ok()) {
        bslib::input_task_button(
          ns("result_next"),
          label = "Get results",
          status = "primary",
          spinner = TRUE,
          state = "ready"
        )
      } else {
        shiny::actionButton(
          ns("result_next"),
          "Get results",
          disabled = TRUE
        )
      }
      
    })
    
    
    # Merge all result tables
    all_results <- shiny::reactive({
      
      purrr::reduce(
        list(
          out_prom(),
          out_homo(),
          out_evdiv(),
          out_funcdiv()
        ),
        full_join,
        by = dplyr::join_by(ID)
      )
      
    })
    
    
    # Optional testing output
    # output$test <- shiny::renderPrint({
    #   list(
    #     avail_loci = locus_data$avail_loci,
    #     avail_type = locus_data$avail_type,
    #     avail_all = locus_data$avail_all,
    #     funcdiv_loci = locus_data$funcdiv_loci,
    #     warn_loci = locus_data$warn_loci,
    #     more_loci = locus_data$more_loci
    #   )
    # })
    
    return(all_results)
    
  })
}