# calculate functional divergence - this is just a placeholder, to generate dummy output

functionalDivergence <- function(middle_table, selected, mean_by = "Locus"){
  
  if (!all(selected %in% c("A", "B", "C"))) stop("wrong locus for funcdiv")
  
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    dplyr::filter(Locus %in% selected) |>
    #tidyr::separate_longer_delim(HLA, "-") |> # separate HLA-II complexes 
    #dplyr::mutate(Locus = ifelse(Locus %in% c("DP", "DQ", "DR"), substr(HLA, 1, 4), Locus)) |>
    dplyr::mutate(Class = ifelse(startsWith(Locus, "D"), "II", "I")) |>
    dplyr::filter(Class != "II") |>  # remove HLA_II alleles
    dplyr::mutate(HLA_ = HLA) |> 
    dplyr::group_by(ID, Class, Locus) |> # locus added, so only the distances within a locus are calculated
    tidyr::expand(HLA, HLA_) |>
    dplyr::rowwise() |>
    dplyr::mutate(TMP = list(sort(c(HLA, HLA_)) ),
                  HLA = TMP[1], 
                  HLA_ = TMP[2]) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::left_join(func_div_data, by = join_by(HLA == Allele1, HLA_ == Allele2)) |> # add HED values
    mutate(FuncDiv = as.numeric(ifelse(HLA==HLA_, 0, FuncDiv))) |> 
    dplyr::select(-TMP)
  
  # arrange to export format
  out <- x |> 
    dplyr::group_by(ID, .data[[mean_by]]) |> # or class -  switch if needed 
    dplyr::summarise(FuncDiv = max(FuncDiv, na.rm = T), .groups = "drop") |> # max,
    tidyr::pivot_wider(names_from = all_of(mean_by) , names_prefix = "FuncDiv_", values_from = "FuncDiv")
  
  return(out)
}