
# modul for selecting and calculating results

resultsUI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::p(texts$result_main),
    shiny::uiOutput(ns("avail_feedback")),
    bslib::layout_columns(
      shiny::checkboxGroupInput(ns("res_ind"), "Allele promiscuity", choices = character()),
      shiny::checkboxGroupInput(ns("res_avg"), "Locus promiscuity", choices = character()), 
      shiny::radioButtons(ns("res_homo"), "Detect homozygosity", choiceNames = c("yes", "no"), choiceValues = c(TRUE, FALSE)) # get homozygosity
    ),
   # shiny::verbatimTextOutput(ns("testing")), # uncomment for easy testing
    bslib::layout_columns(
      shiny::actionButton(ns("result_back"), "Go back", disabled = FALSE),
      shiny::actionButton(ns("result_next"), "Get results!", disabled = TRUE),
      col_widths = c(2, -6, 2, -2)
    )
  )
}

resultsServer <- function(id, middle_table, main_session){
  moduleServer(id, function(input, output, session) {
    
    #get the available loci
    avail_loci <- reactive(sort(unique(middle_table()[["Locus"]])))
    
    # check if any locus are recognized
    avail_check <- reactive(length(avail_loci()) > 0)
    output$avail_feedback <- renderUI(giveFeedBack(avail_check(), texts$results_avail_pass, texts$results_avail_fail ) )
    
    
    #update the checkbox input
    observeEvent(avail_check(), {
      
      if(avail_check()){
        ind_values <- paste0(avail_loci(), "i")
        avg_values <- paste0(avail_loci(), "a")
        
        shiny::updateCheckboxGroupInput(session, "res_ind",
                                        choiceNames = avail_loci(),
                                        choiceValues = ind_values) # add ()?
        shiny::updateCheckboxGroupInput(session, "res_avg",
                                        choiceNames = avail_loci(),
                                        choiceValues = avg_values) # add ()?  
      }
    })
    
    
    #calculate selected  promiscuity values
    res_types <- reactive(c(input$res_ind, input$res_avg)) # merge the values from the two checkboxes
    out_prom <- eventReactive(input$result_next ,{ # only calculate if result_next button is clicked
      hlaFromMiddleTable(middle_table(), res_types())
    })
    
    # calculate heterogeneity 
    out_homo <- eventReactive(input$result_next ,{ # only calculate if result_next button is clicked
      if (input$res_homo) {
        detectHomozygotes(middle_table()) |> as.data.frame()
      } else {
        # a data frame only consisting of the ID column
        middle_table() |>
          dplyr::select(ID) |>
          unique() |> as.data.frame()
      }
      
    })
    
 
    
    #nav buttons
    observeEvent(input$result_back, {
      updateTabsetPanel(main_session, "main_tabs", selected = "select")
    })
    
    
    res_check <- reactive(length(res_types()) > 0)
    observeEvent(res_check(), {
      shiny::updateActionButton(session, "result_next", disabled = !res_check())
    })
    
    
    # merge all results tables
    all_results <- reactive(   dplyr::left_join(out_prom(), out_homo(), by = dplyr::join_by(ID))  )
    
    #output$testing <- renderPrint(out_prom()) # uncomment for easy testing
    return(all_results)
  })
}