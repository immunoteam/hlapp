#' function that calculates divergence
#' 
#' @param middletable middle table in long format, output of selectModul

#calculating evDistance in situ
# evolutionaryDivergence <- function(middle_table, class){
#   
#   if(class == "I"){
#     middle_table = subset(middle_table, !(startsWith(middle_table$Locus, "D")))
#   }else if (class == "II"){
#     middle_table = subset(middle_table, startsWith(middle_table$Locus, "D"))
#   }else{
#     out = "Invalid class"
#   }
#   
#   x <- middle_table |>
#     dplyr::select(ID, HLA, Locus) |>
#     tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes 
#     dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus)) |>
#     dplyr::group_by(ID, Locus) |>
#     dplyr::mutate(Locus = paste0(Locus, "_", dplyr::row_number())) |>
#     tidyr::pivot_wider(id_cols = ID, names_from = Locus, values_from = HLA)
#    #required genotype format for the perl function: columns: ID, Allele1, Allele2, etc
#   
#   write.table(x, "Genotypes.txt", sep = "\t", quote = FALSE, row.names = F)
#   
#   if(class == "I"){
#     out <-  system2('perl', "CalculateIndividualDivergence.pl -d AAdistMatrix_Grantham.cnv -f HLA_ClassI_CWDonly.fas -i Genotypes.txt")
#   }else if (class == "II"){
#     out <-  system2('perl', "CalculateIndividualDivergence.pl -d AAdistMatrix_Grantham.cnv -f HLA_ClassII_CWDonly.fas -i Genotypes.txt")
#   }else{
#     out = "Invalid class"
#   }
#   
#   
#     
#   return(out)
# }


evolutionaryDivergence <- function(middle_table){
  
  mean_by = "Class"
  #mean_by = "Locus"  
  
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes 
    dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus)) |>
    dplyr::mutate(Class = ifelse(startsWith(Locus, "D"), "II", "I")) |>
    dplyr::mutate(HLA_ = HLA) |> 
    dplyr::group_by(ID, Class, Locus) |> # locus added, so only the distances within a locus are calculated
    tidyr::expand(HLA, HLA_) |>
    dplyr::rowwise() |>
    dplyr::mutate(TMP = list(sort(c(HLA, HLA_)) ),
                  HLA = TMP[1], 
                  HLA_ = TMP[2]) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::left_join(evolutionary_distances, by = join_by(HLA == Allele1, HLA_ == Allele2)) |> # add HED values
    mutate(Distance = as.numeric(ifelse(HLA==HLA_, 1, Distance))) |> 
    dplyr::select(-TMP)
  
  # arrange to export format
  x |> 
    dplyr::group_by(ID, .data[[mean_by]]) |> # or class -  switch if needed 
    dplyr::summarise(HED = mean(Distance, na.rm = T)) |> 
    tidyr::pivot_wider(names_from = all_of(mean_by) , names_prefix = "HED_", values_from = "HED")
  
  return(x)
}


#' #' function to create an empty placeholder from middle_table
#' #' 
#' #' @param middle_table middle table
#' 
#' makePlaceHolder <- function(middle_table) {
#'   middle_table |>
#'     dplyr::select(ID) |>
#'     unique() 
# }