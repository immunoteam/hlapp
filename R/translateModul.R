# translate Modul
# part of the modul or can e run separately in translateApp

translateUI <- function(id){
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    p("Here you can translate HLA alleles into the accepted input format of some HLA binding prediction tools."), 
    p("Type in the allele names, (one allele in a row), or import the allele names from the data you uploaded at the Calculate HLA metrics modul"), 
    shiny::textAreaInput(ns("alleles"), label = "Enter alleles here", rows = 6, value = ""), 
    shiny::actionButton(ns("load_alleles"), label = "Import alleles from Calculate metrics modul"), 
    shiny::checkboxGroupInput(ns("formats"), label = "Allele formats", choices = c("NetMHCpan", "MHCnuggets","MixMHCpred") ),
   # shiny::verbatimTextOutput(ns("test")), # for testing
    uiOutput(ns("allele_feedback")),
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
    alleles_vec <- reactive({
      req(input$alleles)
      
      alleles_raw <- gsub(" ", "", unlist(strsplit(input$alleles, split = "\n") ) )
      formatHlaFormat(alleles_raw)
    })
    
    alleles_pass <- reactive({
      alleles_vec()[alleles_vec() %in% unlist(allele_names) ]
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
    
    # check for recognised alleles
    alleles_check <- reactive({
      req(alleles_vec)
      !all( alleles_vec() %in% unlist(allele_names) )
    })
    
    output$allele_feedback <- renderUI({
      giveFeedBack(!alleles_check(), "", "Warning: Some inputs are not recognised as allele names!")
    }) 
    
  })
}
