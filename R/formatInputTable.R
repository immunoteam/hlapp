
#' function to take DQ and DP alleles from a comma separated string and form them into complexes
#'
#'
#'@param x string containing allele names, separated by ", "
#'@param locus DP or DQ
#'@param delim deliminator, , by default
#'
replaceComplex <- function(x, locus, delim = ","){
  if(!locus %in% c("DQ", "DP")) stop("complex locus must be DP or DQ")
  
  # get valuues to be replaced
  x_vec <- strsplit(x, delim) |> unlist()
  vals_to_replace <- grep("-", grep(locus, x_vec, value = T), invert = T,  value = T)
  
  vals_to_keep <- x_vec[!x_vec %in% vals_to_replace]
  
  a <- grep("A", vals_to_replace, value = T )
  b <- grep("B", vals_to_replace, value = T )
  complexes <- expand.grid(a, b)  |>
    dplyr::mutate(comb = paste(Var1, Var2, sep = "-")) |> 
    unique() |> 
    dplyr::pull(comb) 
  
  out <- paste(c(vals_to_keep, complexes), collapse = delim)
  return(out)
}


#'remove forbidden HLA-DQ combinations from datasets
#'
#'@param x dataframe containing acolumn of HLA molecule genotypes
#'@param hla.col name of the column containig hla - genotype
#'@param flag logical, if problematic alleles whould be flagged or removed
#'
#'@return a filtered data.frame, with the same coluns as x
rmForbiddenCombinations <- function(x,  hla.col = "HLA", flag = F ){
  
  x <- x |> 
    dplyr::mutate(AB_comb  = paste(substr(.data[[hla.col]] , 1, 6), 
                                   substr(.data[[hla.col]], 10, 15), 
                                   sep = "-" ))
  
  if(flag) {
    x$flag <- ifelse(x$AB_comb %in% forbidden_DQ_combinations, "forbidden!", "") 
  } else {
    x <-  dplyr::filter(x, !AB_comb %in% forbidden_DQ_combinations)
  }
  
  x$AB_comb <- NULL
  
  return(x)
}


#' formatting manual input - add DQ combination that is robust enough
#' @param x the data frame containing patient data. should contain select.cols
#' @param hla.cols hla clumns
#' @param id.col name of id column
#' @param delim deliminator within coulumn. currently ","
formatManualTable <- function(x, id.col, hla.cols, delim = ","){
  x <- x |>
    dplyr::select(all_of(c(id.col, hla.cols))) |>
    dplyr::rename(ID = .data[[id.col]]) |>
    tidyr::unite(HLA, all_of(hla.cols), sep = delim) 
  
  #  remove HLA or HLA- from the beginning of alleles names
  if (any(grepl("HLA|HLA-", x$HLA) ) ){
    message("Removing HLA from allele names")
    x$HLA <- gsub("HLA|HLA-", "", x$HLA) 
  }
  
  if(any(grepl("\\*|:|_", x$HLA) ) ){
    message("Removing *, _ and : from allele names")
    x$HLA <- gsub("\\*|:|_", "", x$HLA)
  } 
  
  if(delim != " "){
    x <- dplyr::mutate(x, HLA = gsub(" ", "", HLA)) # remove spaces from rows
  } else {
    x <- dplyr::mutate(x, HLA = gsub("  ", " ", HLA)) # if delim is " " remove second spaces from rows
  }
    
  # replace DQ and DP genes with complexes
  x <- x |>
    dplyr::rowwise() |>
    dplyr::mutate(HLA = replaceComplex(HLA, "DQ", delim)) |>
    dplyr::mutate(HLA = replaceComplex(HLA, "DP", delim))
  
  
  #transform the table into a long format
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
formatInputTable <- function(x, id.col = "ID", hla.cols = NULL, flag.forbidden = T, delim = ","){
  
  #turn input table a long format
  x <- formatManualTable(x, id.col, hla.cols, delim)
  
  # check if HLA allele names contain any * or : if they do, remove them
  
  #remove P from the end of the allele name
  if (any(grepl("P$|P-", x$HLA) ) ){
    message("Removing P from end of the allele names")
    x$HLA <- gsub("P$", "", x$HLA) 
    x$HLA <- gsub("P-", "-", x$HLA) 
  }
  
  # remove empty and non identified allele rows
  x <- x |>
    dplyr::filter(HLA != "") |>
    dplyr::filter(grepl("\\d\\d\\d\\d", HLA)) |> #remove rows without 4 digit numbers
    dplyr::mutate(HLA = trim_id(HLA))
  
  

  
  
  #rm forbidden alpha-beta combinations
  x <- rmForbiddenCombinations(x, flag = flag.forbidden)
  x <- dplyr::arrange(x, ID, HLA) 
  
  return(x)
}

# function to remove the 5th and 6th digit from an allele
trim_id <- function(x, types = c("A", "B", "C", "DPB1", "DPA1", "DQA1", "DQB1", "DRB1")) {
  type_pattern <- paste0("(", paste(types, collapse = "|"), ")")
  
  pattern <- paste0(type_pattern, "(\\d{4})\\d{2}")
  sub_pattern <- "\\1\\2"
  
  
  if (any(grepl(pattern, x))) {
    message("Removeing 5th and 6th digits from allele names")
    x <- gsub(pattern, sub_pattern, x)
  }
  return(x)
}
