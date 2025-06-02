

promiscuityUI <- function(id) {
  tagList(
    titlePanel("Promiscuity calculation"),
    sidebarLayout(
      sidebarPanel(
        fileInput(NS(id, "file"), label = " Upload HLA genotype file", accept = c(".tsv", ".csv", ".xlsx")),
        textOutput(NS(id, "resptext") ) ,
        
        selectInput(NS(id, "idvar"), "ID column:", choices = character(1) ),
        checkboxGroupInput(NS(id, "hlavar"), "HLA genotype columns:", choices = character() ),
        
        
        # radioButtons("delim","Deliminator: ",  choices = c(";",",",":"), selected = ";", inline=TRUE),
        #DT::dataTableOutput(NS(id, "input_head")), # show input file

        checkboxGroupInput("type", "Promiscuity to calculate:", 
                           choices = c("HLA-A (individual)" = "A_ind",
                                       "HLA-B (individual" = "B_ind",
                                       "HLA-C (individual)" = "C_ind",
                                       "HLA-DP (individual)" = "DQ_ind",
                                       "HLA-DQ (individual)" = "DQ_ind",
                                       "HLA-DR (individual)" = "DR_ind",
                                       "HLA-A (average)" = "A_avg",
                                       "HLA-B (average" = "B_avg",
                                       "HLA-C (average)" = "C_avg",
                                       "HLA-DP (average)" = "DQ_avg",
                                       "HLA-DQ (average)" = "DQ_avg",
                                       "HLA-DR (average)" = "DR_avg",
                                       "HLA-I" = "HLA_I",
                                       "HLA-II" = 'HLA_II'
                                       ),
                           selected = c("A_avg", "B_avg", "C_avg", "DP_avg", "DQ_avg", "DR_avg")
                           ),
        actionButton(NS(id, "submit"),  "Submit")  # submit input for promiscuity calculation
      ),
      mainPanel(
        fluidRow(
          column(6, h4("Promiscuity of individual HLA-II molecules:"), DT::dataTableOutput(NS(id, "joined_table") ) ), # middle table containing promiscuities of individual alleles
          column(6, h4("Patient promiscuity:"), DT::dataTableOutput(NS(id, "prom_table") ) ) # final output table 
        ),
        fluidRow(
          downloadButton("download_all", "Download all") # TODO - make download button only appear after results are shown
        )
      )
    )


  )
}
