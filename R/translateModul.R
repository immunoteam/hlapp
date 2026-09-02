# translate Modul
# part of the modul or can e run separately in translateApp

translateUI <- function(id){
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    shiny::textAreaInput(ns("alleles"), label = "Enter alleles", rows = 6, value = ""), 
    shiny::actionButton(ns("load_alleles"), label = "Load alleles from PromsicuityApp"), 
    shiny::checkboxGroupInput(ns("formats"), label = "Allele formats", choices = c("netMHCpan", "MHCnuggets","mixMHCpred") ),
   # shiny::verbatimTextOutput(ns("test")), # for testing
    DT::DTOutput(ns("translate_res"))
  )
}

translateServer <- function(id, prom_result = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # gest alleles form promiscuity Modul
    used_alleles <- shiny::reactive({
      shiny::req(prom_result())
      shiny::req(prom_result()())
      
      prom_res <- prom_result()()  # THIS updates automatically
      prom_res$HLA |> unique() |> sort()
    })
    
    shiny::observeEvent(input$load_alleles, {
      shiny::updateTextAreaInput(session , "alleles", value = paste(used_alleles(), collapse = "\n"))
    })
    
    # transform input to vector
    alleles_pass <- reactive({
      req(input$alleles)
      
      alleles_vec <- gsub(" ", "", unlist(strsplit(input$alleles, split = "\n") ) )
      
      alleles_vec[alleles_vec %in% unlist(allele_names) ]
    })
    
    # generate output table
    translated_df <- shiny::reactive({
      shiny::req(input$alleles)
      translateIDs(alleles = alleles_pass(),  formats = input$formats)
    })
      
    
    output$translate_res <- DT::renderDT(translated_df(),
                                   rownames = FALSE,
                                   extensions = "Buttons", 
                                   options = DToptions)
   # output$test<- shiny::renderPrint( alleles_pass() ) # for testing
    
  })
}
