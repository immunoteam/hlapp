#' function to join the HLA genotype and promiscuity
#' @param x data frame of patient HLA genotype. 
#'@param y data frame containig HLA promiscuity values. Output of the fromFilterPromVector function
#'@param hla.col column of x. Contains HLA genotype
joinGenotypePromiscuity <- function(x, y, add.locus = T) {

  if (add.locus) x$Locus <- substr(x$HLA, 1, 2) %>% gsub("\\d", "", .)
  
  res <- left_join(x, y, join_by(HLA) )
  
  return(res)
}

joinGenotypePromiscuity(test_HLA_table1, promiscuity)
