#' function that calculates divergence
#' 
#' @param middletable middle table in long format, output of selectModul
#'
evolutionaryDivergence <- function(middle_table, selected,
                                   mean_by = "Class"){
  
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes 
    dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus)) |>
    dplyr::mutate(Class = ifelse(startsWith(Locus, "D"), "II", "I")) |> 
    dplyr::filter(Class %in% selected) |>
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
    mutate(Distance = as.numeric(ifelse(HLA==HLA_, 0, Distance))) |> 
    dplyr::select(-TMP)
  
  # arrange to export format
  out <- x |> 
    dplyr::group_by(ID, Locus, Class) |> 
    dplyr::summarise(max_dist= max(Distance, na.rm = T), .groups = "drop")     |>
    dplyr::group_by(ID, .data[[mean_by]]) |> 
    dplyr::summarise(HED = mean(max_dist, na.rm = T), .groups = "drop") |> 
    tidyr::pivot_wider(names_from = all_of(mean_by) , names_prefix = "HED_", values_from = "HED")
  
  return(out)
}

