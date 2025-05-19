




#' function to join the HLA genotype and promiscuity
#' @param x data frame of patient HLA genotype. 
#'@param y data frame containig HLA promiscuity values. Output of the fromFilterPromVector function
#'@param hla.col column of x. Contains HLA genotype
joinGenotypePromiscuity <- function(x, y, hla.col = "HLA_II") {
  require(dplyr)
  str.end <- ifelse(hla.col == "HLA_I", 1, 2)
  x$Allele <- sub("[^a-zA-Z]", "", x[, hla.col]) |> substr(1, str.end)
  
  res <- left_join(x, y)
  
  return(res)
}
  
#'calculate the allele and genotype promiscuity for the patients of a cohort
#'
#'@param x the data frame containing the HLA genotypes and promiscuity values of patients, output of joinGenotypePromiscuity
calcGenotypePromiscuity <- function(x){
  require(dplyr) 
  require(tidyr)
  
  res <- x |>
    group_by(ID, Allele) |>
    summarise(Promiscuity = mean(Promiscuity, na.rm = F), .groups = "drop") |>
    pivot_wider(names_from = Allele, values_from = Promiscuity, names_prefix = "Prom_")
  res$Prom_GT = rowMeans(select(res, contains("Prom_")), na.rm = F)
  
  return(res)
}

#' function to respond to if file input is uploaded/received
#' 
#' @param file input$file 
textResponseToInput <- function(file){
  if (is.null(file)) { 
    out <- "No file input, detected. Text input is used detected"
  } 
  else  {
    out <- "File input detected"
  } 
  return(out)
}


