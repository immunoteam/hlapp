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
  load("evolutionary_distances")
  
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes 
    dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus)) |>
    dplyr::mutate(Class = ifelse(startsWith(Locus, "D"), "II", "I")) |>
    dplyr::mutate(HLA_ = HLA) |>
    dplyr::group_by(ID, Class) |>
    tidyr::expand(HLA, HLA_)
    
  #ezt szintén tidyverse-zel kéne, de nem jöttem rá, hogy lehetne :( egy nagyon táblázaton ez így viszonylag sok idő
    for(i in 1:nrow(x)){
      x[i,3:4] = t(as.data.frame(sort(paste(x[i,3:4]))))
    }
  
    x = x[!duplicated(x), ]
  
    x = cbind(x, Distance = NA)
    for(i in 1:nrow(x)){
      if(x$HLA[i] == x$HLA_[i]) x$Distance[i] = 1 
      else if(x$HLA[i] %in% evolutionary_distances$Allele1 & x$HLA_[i] %in% evolutionary_distances$Allele2){
        x$Distance[i] = evolutionary_distances$Distance[(evolutionary_distances$Allele1 == x$HLA[i] & evolutionary_distances$Allele2 == x$HLA_[i])]
      }
    }
  
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