#'calculate the allele and genotype promiscuity for the patients of a cohort
#'
#'@param x the data frame containing the HLA genotypes and promiscuity values of patients, output of joinGenotypePromiscuity
calcPromiscuity <- function(x){
  
  res <- x |>
    group_by(ID, Locus) |>
    summarise(Promiscuity = mean(Promiscuity, na.rm = F), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Locus, values_from = Promiscuity, names_prefix = "Prom_")
  res$Prom_GT_I = rowMeans(select(res, all_of( c("Prom_A", "Prom_B", "Prom_C") ) ), na.rm = F)
  res$Prom_GT_II = rowMeans(select(res, contains("Prom_D")), na.rm = F)
  
  return(res)
}
