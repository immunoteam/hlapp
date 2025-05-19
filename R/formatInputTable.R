#' function to recognize the format of input data 
#'  needs to be updated as further formats are included in the app
#'  
#'  @param x data frame
recogniseInputFormat <- function(x) {
  case <- "unknown"
  suppressWarnings(
    if( all(colnames(x) == c("ID", "HLA_I", "HLA_II") ) ) case <- "cohort"
  )
  
  switch(case,
         cohort = message(paste("Table format recognised:", case)),
         validate("Unrecognised HLA table format")
         )
  
  return(case)
}

formatCohortTable <- function(x, delim = ",") {
  select_cols <- c("ID", "HLA_I", "HLA_II")
  
  x |>
    select(all_of(select_cols) ) |>
    mutate(HLA_allele = paste(HLA_I, HLA_II, sep = ",")) |>
    separate_longer_delim(cols = all_of("HLA_allele"), delim = delim ) |>
    select(ID, HLA_allele)
}


# functions to format input from cohorts and to calculate HLA-II promiscuity

#'remove forbidden HLA-DQ combinations from datasets
#'
#'@param x dataframe containing acolumn of HLA molecule genotypes
#'@param hla.col name of the column containig hla - genotype
#'
#'@return a filtered data.frame, with the same coluns as x
rmForbiddenCombinations <- function(x, 
                                    hla.col = "HLA_II"){
  require(dplyr)
  
  x$TMP <- x[ , hla.col]
  x <- x |> 
    mutate(AB_comb  = paste(substr(TMP, 1, 6), 
                            substr(TMP, 10, 15), 
                            sep = "-" )) |>
    filter(!AB_comb %in% forbidden_DQ_combinations) |>
    select(!all_of(c("TMP", "AB_comb" ) ) )
  
  return(x)
}

#' format patient HLA genotype into a optimal format for promiscuity calculation
#' 
#' @param x the data frame containing patient data. should contain select.cols
#' @param select.cols a char. vector of two. the first element should be the name of the column containing patient ID, and the second element should be the name of the column that contains the HLA genotype of interest.
#' @param delim character value to separate the individual HLA alleles by
#' @param rm.forbidden whether to remove forbidden HLAQ alpha-beta combinations for the table. TRUE by default.
#' 
#' @return a data frame
formatInputTable <- function(x, rm.forbidden = T){
  
  case <-recogniseInputFormat(x)
  
  x <- switch(case, 
         cohort = formatCohortTable(x)
         )
  
  #rm forbidden alpha-beta combinations
  if (rm.forbidden) {
    x <- rmForbiddenCombinations(x, hla.col = "HLA_allele")
  }
  
  return(x)
}


