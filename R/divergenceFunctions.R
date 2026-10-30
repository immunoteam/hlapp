# get alla avilable divergence values for a middle table
getAllDivergence <- function(middle_table, div_type = "FuncDiv", mean_by = "Locus") {
  if (div_type == "FuncDiv") {
    div_data <- func_div_data
  } else if (div_type == "HED") {
    div_data <- evolutionary_distances
  } else {
    stop("div_type must be FuncDiv or HED")
  }

  x <- middle_table |>
    dplyr::select(ID, HLA, .data[[mean_by]]) |>
    # dplyr::filter(.data[[mean_by]] %in% selected) |>
    dplyr::rowwise() |>
    dplyr::mutate(HLA = ifelse(grepl("DQ", HLA), gsub("DQA.+-", "", HLA), HLA)) |> # if DQ allele, remove DQA part of the complex
    unique() |>
    dplyr::mutate(HLA_ = HLA) |>
    dplyr::group_by(ID, .data[[mean_by]]) |> # locus added, so only the distances within a locus are calculated
    tidyr::expand(HLA, HLA_) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      TMP = list(sort(c(HLA, HLA_))),
      HLA = TMP[1],
      HLA_ = TMP[2]
    ) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::filter(length(TMP) == 1 | HLA_ != HLA) |>
    dplyr::left_join(div_data, by = join_by(HLA == Allele1, HLA_ == Allele2)) |> # add HED values
    dplyr::mutate(!!div_type := round(as.numeric(ifelse(HLA == HLA_, 0, .data[[div_type]])), 3)) |>
    dplyr::select(-TMP)

  # arrange to export format
  out <- x |>
    dplyr::select(all_of(c("ID", mean_by, div_type))) |>
    tidyr::pivot_wider(names_from = all_of(mean_by), names_prefix = paste0(div_type, "_"), values_from = div_type)

  return(out)
}

# function to mutate column, if an allele is missing
mutateToWarning <- function(df, cols_to_warning) {
  df[, cols_to_warning] <- "Warning: Cannot be calculated"
  return(df)
}

# select the selected functional divergence values
functionalDivergence <- function(middle_table, selected, warn = c()) {
  if (!all(selected %in% c("A", "B", "C", "class-I"))) stop("wrong locus for funcdiv")

  selected_loc <- intersect(selected, c("A", "B", "C"))
  selected_class <- intersect(selected, c("class-I"))

  middle_table <- middle_table |>
    dplyr::filter(Locus %in% c("A", "B", "C")) # remove funcdiv values that will not be calculated anyway.

  out_df <- getAllDivergence(middle_table, div_type = "FuncDiv")

  if ("class-I" %in% selected_class) {
    out_df$FuncDiv_class_I <- round(rowMeans(dplyr::select(out_df, any_of(c("FuncDiv_A", "FuncDiv_B", "FuncDiv_C"))), na.rm = TRUE), 3)
  }


  # replace calculated values to warning if necessary
  if (length(warn) > 0) {
    if (any(c("A", "B", "C") %in% warn)) warn <- c(warn, "class_I")
    cols_to_warning <- paste0("FuncDiv_", warn)
    out_df <- mutateToWarning(out_df, cols_to_warning)
  }

  # subset only to selected columns
  out_df <- out_df |> dplyr::select(ID, paste0("FuncDiv_", gsub("-", "_", selected)))

  return(out_df)
}

# select the selected functional divergence values
evolutionaryDivergence <- function(middle_table, selected, warn = c()) {
  selected_loc <- intersect(selected, c("A", "B", "C", "DP", "DQ", "DR"))
  selected_class <- intersect(selected, c("class-I", "class-II"))

  out_df <- getAllDivergence(middle_table, div_type = "HED")

  if ("class-I" %in% selected_class) {
    out_df$HED_class_I <- round(rowMeans(dplyr::select(out_df, any_of(c("HED_A", "HED_B", "HED_C"))), na.rm = TRUE), 3)
  }
  if ("class-II" %in% selected_class) {
    out_df$HED_class_II <- round(rowMeans(dplyr::select(out_df, any_of(c("HED_DP", "HED_DQ", "HED_DR"))), na.rm = TRUE), 3)
  }


  # replace calculated values to warning if necessary
  if (length(warn) > 0) {
    if (any(c("A", "B", "C") %in% warn)) warn <- c(warn, "class_I")
    if (any(c("DP", "DQ", "DR") %in% warn)) warn <- c(warn, "class_II")
    cols_to_warning <- paste0("HED_", warn)
    out_df <- mutateToWarning(out_df, cols_to_warning)
  }

  # subset only to selected columns
  out_df <- out_df |> dplyr::select(ID, paste0("HED_", gsub("-", "_", selected)))


  return(out_df)
}
