
# modul for selecting and calculating results

########### UI ################xxx

resultsUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(texts$result_main),
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
      shiny::radioButtons(ns("res_homo"), "Detect homozygosity", choiceNames = c("yes", "no"), choiceValues = c(TRUE, FALSE), selected = FALSE), # get homozygosity
      shiny::checkboxGroupInput(ns("res_evdiv"), "Calculate evolutionary divergence", choiceNames = c("HLA-I", "HLA-II"), choiceValues = c("I", "II")), # get evolutionary divergence
      shiny::checkboxGroupInput(ns("res_funcdiv"), "Calculate functional divergence", choiceNames = c("HLA-A", "HLA-B", "HLA-C"), choiceValues = c("A", "B", "C")) # get evolutionary divergence
      
    ),
   # shiny::verbatimTextOutput(ns("testing")), # uncomment for easy testing
    bslib::layout_columns(
      shiny::actionButton(ns("result_back"), "Go back", disabled = FALSE),
      shiny::uiOutput(ns("results_next_ui")),
      col_widths = c(2, -6, 2, -2)
    )
   , verbatimTextOutput(ns("test"))
  )
}

########## SERVER ################

###### Control input UI ######
resultsServer <- function(id, middle_table, prom_session){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    
    #get the available loci
    avail_loci <- shiny::reactive(sort(unique(middle_table()[["Locus"]])))
    
    # get loci, where none of the samples has more than one 
    warn_loci <- reactive(checkFor2Alleles(middle_table() ) )
    more_loci <- reactive(checkForMoreAlleles(middle_table() ) )
    
    # check if any locus are recognized - give user feedback
    avail_check <- shiny::reactive(length(avail_loci()) > 0)
    output$avail_feedback <- shiny::renderUI(giveFeedBack(avail_check(), paste(texts$results_avail_pass, paste(avail_loci(), collapse = ", ")), texts$results_avail_fail ) )
    output$warning_feedback <- renderUI(giveFeedBack(length(warn_loci()) == 0, "",
                                                     paste("Warning: The following HLA-loci has only one allele in the input:", 
                                                           paste(warn_loci(), collapse = ", "),
                                                           "---Some results will not be calculated."
                                                           )
                                                     ))
    output$more_feedback <- renderUI(giveFeedBack(length(more_loci()) == 0, "",
                                                     paste("ERROR!: The following HLA-loci has more than 2 alleles in the input:", 
                                                           paste(more_loci(), collapse = ", "),
                                                          "---Fix this to proceed!"
                                                     )
    ))
    
    
    
    #update the checkbox input based on available values 
    shiny::observeEvent(avail_check(), {
      
      if(avail_check()){
        ind_values <- paste0(avail_loci(), "i")
        avg_values <- paste0(avail_loci(), "a")
        avail_type <- dplyr::recode_values(avail_loci(),
                                                         c("A", "B", "C") ~ "I", 
                                                         c("DP", "DQ", "DR") ~ "II")  |>  unique()
        
        shiny::updateCheckboxGroupInput(session, "res_ind",
                                        choiceNames = avail_loci(),
                                        choiceValues = ind_values) # add ()?
        shiny::updateCheckboxGroupInput(session, "res_avg",
                                        choiceNames = avail_loci(),
                                        choiceValues = avg_values) # add ()?  
        
        updateCheckboxGroupInput(session, "res_funcdiv", 
                                 choiceNames = paste0("HLA-", avail_loci()), 
                                 choiceValues = avail_loci()
                                 )
        updateCheckboxGroupInput(session, "res_evdiv", 
                                 choiceNames = paste0("HLA-", avail_type), 
                                 choiceValues = avail_type
                                 )
        
      }
    })
    
    # update checked boxes adter select all / deselect all is clicked
    shiny::observeEvent(input$res_ind_all, {
      ind_values <- paste0(avail_loci(), "i")
      if (input$res_ind_all) {
        shiny::updateCheckboxGroupInput( session, "res_ind", selected = ind_values)
      } else {
        shiny::updateCheckboxGroupInput(session, "res_ind", selected = character(0))
      }
    })
    
    shiny::observeEvent(input$res_avg_all, {
      avg_values <- paste0(avail_loci(), "a")
      if (input$res_avg_all) {
        shiny::updateCheckboxGroupInput( session, "res_avg", selected = avg_values)
      } else {
        shiny::updateCheckboxGroupInput(session, "res_avg", selected = character(0))
      }
    })
    
###### Generate results ###### 
    
    #calculate selected  promiscuity values
    res_types <- shiny::reactive(c(input$res_ind, input$res_avg)) # merge the values from the two checkboxes
    
    out_prom <- shiny::eventReactive(input$result_next ,{ # only calculate if result_next button is clicked
      #Sys.sleep(3)# for testing
      hlaFromMiddleTable(middle_table(), res_types())
    })
    
    # calculate heterogeneity 
    out_homo <- shiny::eventReactive(input$result_next ,{ # only calculate if result_next button is clicked
      if (input$res_homo) {
        detectHomozygotes(middle_table(), warn = warn_loci()) |> 
          as.data.frame()
      } else {
        # a data frame only consisting of the ID column
        makePlaceHolder(middle_table())
      }
    })
    
    # calculate evolutionary divergence
    out_evdiv <- shiny::eventReactive(input$result_next ,{ # only calculate if result_next button is clicked
      if (length(input$res_evdiv) > 0) {
        evolutionaryDivergence(middle_table() , mean_by = "Class", selected = input$res_evdiv, warn = warn_loci()) |> 
          as.data.frame()
      } else {
        # a data frame only consisting of the ID column
        makePlaceHolder(middle_table())
      }
    })
    
    out_funcdiv <-  shiny::eventReactive(input$result_next ,{ # only calculate if result_next button is clicked
      if (length(input$res_funcdiv) > 0) {
        functionalDivergence(middle_table(), selected = input$res_funcdiv, "Locus", warn = warn_loci()) |> 
          as.data.frame()
      } else {
        # a data frame only consisting of the ID column
        makePlaceHolder(middle_table())
      }
    })
 
    #nav buttons
    shiny::observeEvent(input$result_back, {
      shiny::updateTabsetPanel(prom_session, "prom_tabs", selected = "select")
    })
    
    
    # RESULTS nav button - only active if select_check is passed&
    res_check_ok <- shiny::reactive( any( length(res_types()) > 0, input$res_homo, length(input$res_evdiv) > 0, length(input$res_funcdiv) > 0  ) & length(more_loci()) == 0 )
    
    output$results_next_ui  <- shiny::renderUI(
      if (res_check_ok()) {
        bslib::input_task_button(ns("result_next"), label = "Get results", status = "primary" , spinner = TRUE, state = "ready")
      } else {
        shiny::actionButton(ns("result_next"), "Get results", disabled = TRUE)
      }
    )
    
    
    # merge all results tables
    all_results <- shiny::reactive(  purrr::reduce(list( out_prom(), out_homo(), out_evdiv(), out_funcdiv() ), full_join, by = dplyr::join_by(ID) ) )
    
    
    
    output$test <- shiny::renderPrint(list(warn_loci(), more_loci(), res_check_ok())) # uncomment for easy testing
    return(all_results)
  })
}
