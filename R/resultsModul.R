
# modul for selecting and calculating results

########### UI ################xxx

resultsUI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::p(texts$result_main),
    shiny::uiOutput(ns("avail_feedback")),
    bslib::layout_columns(
      shiny::div(
        p("Allele promiscuity"),
        shiny::checkboxInput(ns("res_ind_all"),  "Select / Deselect all", value = FALSE),
        shiny::checkboxGroupInput(ns("res_ind"), NULL, choices = character())
        ),
      shiny::div(
        p("Locus promiscuity"),
        shiny::checkboxInput(ns("res_avg_all"),  "Select / Deselect all", value = FALSE),
        shiny::checkboxGroupInput(ns("res_avg"), NULL, choices = character())  
      ),
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

########## SERVER ################

###### Control input UI ######
resultsServer <- function(id, middle_table, main_session){
  moduleServer(id, function(input, output, session) {
    
    #get the available loci
    avail_loci <- reactive(sort(unique(middle_table()[["Locus"]])))
    
    # check if any locus are recognized
    avail_check <- reactive(length(avail_loci()) > 0)
    output$avail_feedback <- renderUI(giveFeedBack(avail_check(), texts$results_avail_pass, texts$results_avail_fail ) )
    
    
    #update the checkbox input based on available values 
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
    
    # update checked boxes adter select all / deselect all is clicked
    observeEvent(input$res_ind_all, {
      ind_values <- paste0(avail_loci(), "i")
      if (input$res_ind_all) {
        updateCheckboxGroupInput( session, "res_ind", selected = ind_values)
      } else {
        updateCheckboxGroupInput(session, "res_ind", selected = character(0))
      }
    })
    
    observeEvent(input$res_avg_all, {
      avg_values <- paste0(avail_loci(), "a")
      if (input$res_avg_all) {
        updateCheckboxGroupInput( session, "res_avg", selected = avg_values)
      } else {
        updateCheckboxGroupInput(session, "res_avg", selected = character(0))
      }
    })
    
###### Generate results ###### 
    
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
        makePlaceHolder(middle_table())
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
    all_results <- reactive(  purrr::reduce(list( out_prom(), out_homo() ), full_join, by = dplyr::join_by(ID) ) )
    
    #output$testing <- renderPrint(out_prom()) # uncomment for easy testing
    return(all_results)
  })
}