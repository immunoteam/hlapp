#' function that detect hla homozygote patients 
#' 
#' @param middletable middle table in long format, output of selectModul

detectHomozygotes <- function(middle_table){

    middle_table |>
      dplyr::group_by(ID, Locus) |>
      dplyr::summarise(Homozygote = dplyr::n_distinct(HLA) == 1, .groups = "drop") |>
      tidyr::pivot_wider(names_from = Locus, values_from = Homozygote, names_prefix = "Homozygote_")

}
