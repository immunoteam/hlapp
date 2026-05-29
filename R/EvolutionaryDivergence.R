#' function that calculates divergence
#' 
#' @param middle_table middle table in long format, output of selectModul

evolutionaryDivergence <- function(middle_table){
  
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes 
    dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus) )
  
  out <-  x |>
    dplyr::group_by(ID, Locus) #|>
    #[calculation]
    
  return(out)
}


#' function to create an empty placeholder from middle_table
#' 
#' @param middle_table middle table

makePlaceHolder <- function(middle_table) {
  middle_table |>
    dplyr::select(ID) |>
    unique() 
}