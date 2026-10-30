#' function to take DQ and DP alleles from a comma separated string and form them into complexes
#'
#'
#' @param x string containing allele names, separated by ", "
#' @param locus DP or DQ
#' @param delim deliminator, , by default
#'
replaceComplex <- function(x, locus, delim = ",") {
  if (!locus %in% c("DQ", "DP")) stop("complex locus must be DP or DQ")

  # get values to be replaced
  x_vec <- strsplit(x, delim) |> unlist()
  vals_to_replace <- grep("-", grep(locus, x_vec, value = T), invert = T, value = T)

  vals_to_keep <- x_vec[!x_vec %in% vals_to_replace]

  a <- grep("A", vals_to_replace, value = T)
  b <- grep("B", vals_to_replace, value = T)
  complexes <- expand.grid(a, b) |>
    dplyr::mutate(comb = paste(Var1, Var2, sep = "-")) |>
    unique() |>
    dplyr::pull(comb)

  out <- paste(c(vals_to_keep, complexes), collapse = delim)
  return(out)
}


#' remove forbidden HLA-DQ combinations from datasets
#'
#' @param x dataframe containing acolumn of HLA molecule genotypes
#' @param hla.col name of the column containig hla - genotype
#' @param flag logical, if problematic alleles whould be flagged or removed
#'
#' @return a filtered data.frame, with the same coluns as x
rmForbiddenCombinations <- function(x, hla.col = "HLA", flag = F) {
  x <- x |>
    dplyr::mutate(AB_comb = paste(substr(.data[[hla.col]], 1, 6),
      substr(.data[[hla.col]], 10, 15),
      sep = "-"
    ))

  if (flag) {
    x$flag <- ifelse(x$AB_comb %in% forbidden_DQ_combinations, "forbidden!", "")
  } else {
    x <- dplyr::filter(x, !AB_comb %in% forbidden_DQ_combinations)
  }

  x$AB_comb <- NULL

  return(x)
}


#' formatting manual input - add DQ combination that is robust enough
#' @param x the data frame containing patient data. should contain select.cols
#' @param hla.cols hla clumns
#' @param id.col name of id column
#' @param delim deliminator within coulumn. currently ","
formatManualTable <- function(x, id.col, hla.cols, delim = ",") {
  # merge all hla columns into one column, values separated by delim
  x <- x |>
    dplyr::select(all_of(c(id.col, hla.cols))) |>
    dplyr::rename(ID = .data[[id.col]]) |>
    tidyr::unite(HLA, all_of(hla.cols), sep = delim)

  x <- x |>
    dplyr::mutate(HLA = formatHlaFormat(HLA))

  # replace DQ and DP genes with complexes - this works on a delim separated, single charachter
  x <- x |>
    dplyr::rowwise() |>
    dplyr::mutate(HLA = replaceComplex(HLA, "DQ", delim)) |>
    dplyr::mutate(HLA = replaceComplex(HLA, "DP", delim))


  # transform the table into a long format
  x <- x |>
    tidyr::separate_longer_delim(HLA, delim = delim) |>
    dplyr::arrange(ID, HLA)

  return(x)
}

#' format patient HLA genotype into a optimal format for promiscuity calculation
#'
#' @param x the data frame containing patient data. should contain select.cols
#' @param hla.cols hla clumns
#' @param id.col name of id column
#' @param flag.forbidden whether to remove forbidden HLAQ alpha-beta combinations for the table. TRUE by default.
#'
#' @return a data frame
formatInputTable <- function(x, id.col = "ID", hla.cols = NULL, flag.forbidden = T, delim = ",") {
  # force ID to be treated as a character
  x[[id.col]] <- as.character(x[[id.col]])

  # turn input table a long format
  x <- formatManualTable(x, id.col, hla.cols, delim)

  # remove empty and non identified allele rows
  x <- x |>
    dplyr::filter(HLA != "") |>
    dplyr::filter(grepl("\\d\\d\\d\\d", HLA)) # remove rows without 4 digit numbers


  # rm forbidden alpha-beta combinations
  x <- rmForbiddenCombinations(x, flag = flag.forbidden)
  x <- dplyr::arrange(x, ID, HLA)

  return(x)
}
