#' function to join the HLA genotype and promiscuity
#' @param x data frame of patient HLA genotype. 
#'@param y data frame containig HLA promiscuity values. Output of the fromFilterPromVector function
#'@param add.locus TRUE
joinGenotypePromiscuity <- function(x, y, add.locus = T) {

  if (add.locus) x$Locus <- substr(x$HLA, 1, 2) %>% gsub("\\d", "", .)
  
  res <- dplyr::left_join(x, y, dplyr::join_by(HLA) ) #|>
    #dplyr::mutate(ID = paste0("id_", ID)) 
  
  res <- dplyr::filter(res, Locus %in% c("A", "B", "C", "DP", "DQ", "DR")) # only allow sepected types
  
  message("Middle table is succesful")
  
  return(res)
}
