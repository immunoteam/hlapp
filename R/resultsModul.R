
# modul for selecting and calculating results

resultsUI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::h3("Select values to be calculated:"),
    bslib::layout_columns(
      shiny::checkboxGroupInput(ns("res_ind"), "Allele promiscuity", choices = character()),
      shiny::checkboxGroupInput(ns("res_avg"), "Locus promiscuity", choices = character())
    )  
  )
}

resultsServer <- function(id, middle_table, main_session){
  moduleServer(id, function(input, output, session) {
    
    #get the available loci
    avail_loci <- reactive(sort(unique(middle_table()[["Locus"]])))
    
    #update the checkbox inputs
    observeEvent(avail_loci(), {
      
      ind_values <- paste0(avail_loci(), "i")
      avg_values <- paste0(avail_loci(), "a")
      
      shiny::updateCheckboxGroupInput(session, "res_ind",
                                      choiceNames = avail_loci(),
                                      choiceValues = ind_values) # add ()?
      shiny::updateCheckboxGroupInput(session, "res_avg",
                                      choiceNames = avail_loci(),
                                      choiceValues = avg_values) # add ()?
    })
    
    
    #calculate selected values
    res_types <- reactive(c(input$res_ind, input$res_avg)) # merge the values from the two checkboxes
    out_prom <- eventReactive(res_types() ,{
      hlaFromMiddleTable(middle_table(), res_types())
    })
    
    
    
    return(out_prom)
  })
}