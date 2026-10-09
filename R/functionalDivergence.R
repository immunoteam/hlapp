# calculate functional divergence - this is just a placeholder, to generate dummy output

functionalDivergence <- function(middle_table, selected, mean_by = "Locus", warn){
  
  if (!all(selected %in% c("A", "B", "C"))) stop("wrong locus for funcdiv")
  
  x <- middle_table |>
    dplyr::select(ID, HLA, Locus) |>
    dplyr::filter(Locus %in% selected) |>
    dplyr::mutate(HLA_ = HLA) |> 
    dplyr::group_by(ID, Locus) |> # locus added, so only the distances within a locus are calculated
    tidyr::expand(HLA, HLA_) |>
    dplyr::rowwise() |>
    dplyr::mutate(TMP = list(sort(c(HLA, HLA_)) ),
                  HLA = TMP[1], 
                  HLA_ = TMP[2]) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::filter(length(TMP) == 1 | HLA_ != HLA ) |> 
    dplyr::left_join(func_div_data, by = join_by(HLA == Allele1, HLA_ == Allele2)) |> # add HED values
    dplyr::mutate(FuncDiv = round(as.numeric(ifelse(HLA==HLA_, 0, FuncDiv)), 3 ) ) |> 
    dplyr::select(-TMP)
  
  # arrange to export format
  out <- x |> 
    mutateToWarning(warn, "FuncDiv") |> 
    tidyr::pivot_wider(names_from = all_of(mean_by) , names_prefix = "FuncDiv_", values_from = "FuncDiv") |> 
    dplyr::select(ID, contains("FuncDiv"))
  
  return(out)
}