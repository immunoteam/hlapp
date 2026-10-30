#' function that detect hla homozygote patients
#'
#' @param middle_table middle table in long format, output of selectModul

detectHomozygotes <- function(middle_table, warn) {
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes
    dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus))

  out <- x |>
    dplyr::group_by(ID, Locus) |>
    dplyr::summarise(Homozygote = dplyr::n_distinct(HLA) == 1, .groups = "drop") |>
    mutateToWarning(warn, "Homozygote") |>
    tidyr::pivot_wider(names_from = Locus, values_from = Homozygote, names_prefix = "Homozygote_")

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
