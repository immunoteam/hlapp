# translate Modul
# part of the modul or can e run separately in translateApp

translateUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    textAreaInput(ns("alleles"), label = "Enter alleles", rows = 6, value = ""), 
    actionButton(ns("load_alleles"), label = "Load alleles from PromsicuityApp"), 
    checkboxGroupInput(ns("formats"), label = "Allele formats", choices = c("netMHCpan", "MHCnuggets","mixMHCpred") ),
    #shiny::verbatimTextOutput(ns("test")),
    DT::DTOutput(ns("prom_res"))
  )
}

translateServer <- function(id, prom_result = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # gest alleles form promiscuity Modul
    used_alleles <- reactive({
      req(prom_result())
      req(prom_result()())
      
      prom_res <- prom_result()()  # THIS updates automatically
      prom_res$HLA |> unique() |> sort()
    })
    
    observeEvent(input$load_alleles, {
      updateTextAreaInput(session , "alleles", value = paste(used_alleles(), collapse = "\n"))
    })
    
    # generate output table
    translated_df <- reactive({
      req(used_alleles())
      translateIDs(alleles = used_alleles(), formats = input$formats, hla_names = hla_names )
    })
      
    
    output$prom_res <- DT::renderDT( translated_df() ) 
    #output$test<- shiny::renderPrint( used_alleles() ) 
    
  })
}