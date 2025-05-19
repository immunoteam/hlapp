#' format patient HLA genotype into a optimal format for promiscuity calculation
#' 
#' @param x the data frame containing patient data. should contain select.cols
#' @param select.cols a char. vector of two. the first element should be the name of the column containing patient ID, and the second element should be the name of the column that contains the HLA genotype of interest.
#' @param delim character value to separate the individual HLA alleles by
#' @param rm.forbidden wether to remove forbidden HLAQ alpha-beta combinations for the table. TRUE by default.
#' 
#' @return a data frame
formatCohortTable <- function(x, 
                              select.cols = c("ID","HLA_II"),
                              delim = ",",
                              rm.forbidden = T){
  require(dplyr)
  #format df
  x <- x |>
    select(all_of(select.cols) ) |>
    separate_longer_delim(cols = all_of(select.cols[2] ), delim = delim )
  #rm forbidden alpha-beta combinations
  if (rm.forbidden) {
    x <- rmForbiddenCombinations(x, hla.col = select.cols[2])
  }
  
  return(x)
}