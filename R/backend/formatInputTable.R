#' function to recognize the format of input data 
#'  needs to be updated as further formats are included in the app
#'  
#'  @param x data frame
recogniseInputFormat <- function(x) {
  
  cohort_cols <- c("ID", "HLA_I", "HLA_II") 
  standard_cols <- c("A1", "A2", "B1", "B2", "C1", "C2", "
                     DPA11", "DPA12", "DPB11", "DPB12", "DQA11", "DQA12", "DQB11",
                     "DRB11", "DRB12" )
  
  case <- "unknown"
  
  if(  all(colnames(x) %in% cohort_cols)  ){ case <- "cohort" } 
    else if ( any(colnames(x) %in% standard_cols ) ) { case <- "standard" }
  
  if (case != "unknown") message(paste("Table format recognised:", case))
  else stop("Unrecognised HLA table format")
  
  return(case)
}

#' function to take DQ and DP alleles from a comma separated string and form them into complexes
#'
#'
#'@param x string containing allele names, separated by ", "
#'@param locus DP or DQ
#'
replaceComplex <- function(x, locus){
  if(!locus %in% c("DQ", "DP")) stop("complex locus must be DP or DQ")
  
  # get valuues to be replaced
  x_vec <- strsplit(x, ", ") |> unlist()
  vals_to_replace <- grep("-", grep(locus, x_vec, value = T), invert = T,  value = T)
  
  vals_to_keep <- x_vec[!x_vec %in% vals_to_replace]
  
  a <- grep("A", vals_to_replace, value = T )
  b <- grep("B", vals_to_replace, value = T )
  complexes <- expand.grid(a, b)  |>
    dplyr::mutate(comb = paste(Var1, Var2, sep = "-")) |> 
    unique() |> 
    dplyr::pull(comb) 
  
  out <- paste(c(vals_to_keep, complexes), collapse = ", ")
  return(out)
}


#'remove forbidden HLA-DQ combinations from datasets
#'
#'@param x dataframe containing acolumn of HLA molecule genotypes
#'@param hla.col name of the column containig hla - genotype
#'
#'@return a filtered data.frame, with the same coluns as x
rmForbiddenCombinations <- function(x, 
                                    hla.col = "HLA", flag = F ){
  require(dplyr)
  
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



# function to transform cohort format
formatCohortTable <- function(x, delim = ",") {
  select_cols <- c("ID", "HLA_I", "HLA_II")
  
  x |>
    dplyr::select(tidyr::all_of(select_cols) ) |>
    dplyr::mutate(HLA = paste(HLA_I, HLA_II, sep = ",")) |>
    tidyr::separate_longer_delim(cols = tidyr::all_of("HLA"), delim = delim ) |>
    dplyr::select(ID, HLA)
}




# function to transform the data from the table format tha contains the hla i and hla ii genotypes in two columns, divided by commas
formatStandardTable <- function(x){
  
  #get A, B, C and DR alleles
  getABCDR <- function(x, locus){
    a1 <- x[grep(paste0("^", locus, "1"),  names(x)) ] |> as.character()
    a2 <- x[grep(paste0("^", locus, "2"),  names(x)) ] |> as.character()
    
    paste(a1, a2, sep = ", ")
  }
  

  # combine HLA_DP and DR alleles
  combineDPDQ <- function(x, locus){
    
    a <- x[grep(paste0(locus, "A1"), names(x)) ]
    b <- x[grep(paste0(locus, "B1"), names(x)) ]
    
    expand.grid(a, b) |> 
      dplyr::mutate(comb = paste(Var1, Var2, sep = "-")) |> 
      unique() |> 
      dplyr::pull(comb) |>
      paste(collapse = ", ")
  }

  out <- data.frame(ID = x[, 1],
             A = apply(x, 1, getABCDR, locus = "A"),
             B = apply(x, 1, getABCDR, locus = "B"),
             C = apply(x, 1, getABCDR, locus = "C"),
             DP = apply(x, 1, combineDPDQ, locus = "DP"),
             DQ = apply(x, 1, combineDPDQ, locus = "DQ"),
             DR = apply(x, 1, getABCDR, locus = "DRB1")
                    ) 
   out <- out |> 
     dplyr::mutate(HLA = paste(A, B, C, DP, DQ, DR, sep = ", ")) |>
     dplyr::select(ID, HLA) |>
     tidyr::separate_longer_delim(cols = all_of("HLA"), delim = ", ")
     
  return(out)
}






# formatting manual input - add DQ combination that is robust enough
formatManualTable <- function(x, id.col, hla.cols, delim = ", "){
  x <- x |>
    dplyr::select(all_of(c(id.col, hla.cols))) |>
    dplyr::rename(ID = .data[[id.col]]) |>
    tidyr::unite(HLA, all_of(hla.cols), sep = delim) 
  
  # replace DQ and DP genes with complexes
  x <- x |>
    dplyr::rowwise() |>
    dplyr::mutate(HLA = replaceComplex(HLA, "DQ")) |>
    dplyr::mutate(HLA = replaceComplex(HLA, "DP"))
  
  
  #transform the table into a long format
  x <- x |>
    tidyr::separate_longer_delim(HLA, delim = delim) |>
    dplyr::arrange(ID, HLA)
  
  return(x)
}




#' format patient HLA genotype into a optimal format for promiscuity calculation
#' 
#' @param x the data frame containing patient data. should contain select.cols
#' @param select.cols a char. vector of two. the first element should be the name of the column containing patient ID, and the second element should be the name of the column that contains the HLA genotype of interest.
#' @param delim character value to separate the individual HLA alleles by
#' @param rm.forbidden whether to remove forbidden HLAQ alpha-beta combinations for the table. TRUE by default.
#' 
#' @return a data frame
formatInputTable <- function(x, flag.forbidden = T, 
                             manual = F, id.col = "ID", hla.cols = NULL){
  
  
  if(manual) {
    case <- "manual"
   # validate(!is.null(id.col), !is.null(hla.cols))
  } else {
    case <-recogniseInputFormat(x) # decide the type of input data based on column names
  }
  
  
  #turn input table into the two tow format
  x <- switch(case, 
         manual = formatManualTable(x, id.col, hla.cols),
         cohort = formatCohortTable(x),
         standard = formatStandardTable(x)
         )
  
  # check if HLA allele names contain any * or : if they do, remove them
  
  if(any(grepl("\\*|:", x$HLA) ) ){
    message("Removing * and : from allele names")
    x$HLA <- gsub("\\*|:", "", x$HLA)
  } 
  
  #remove P from the end of the allele name
  if (any(grepl("P$|P-", x$HLA) ) ){
    message("Removing P from end of the allele names")
    x$HLA <- gsub("P$", "", x$HLA) 
    x$HLA <- gsub("P-", "-", x$HLA) 
  }
  
  # remove empty and non identified allele rows
  x <- x |>
    dplyr::filter(HLA != "") |>
    dplyr::filter(grepl("\\d\\d\\d\\d", HLA))
  
  #rm forbidden alpha-beta combinations
  x <- rmForbiddenCombinations(x, flag = flag.forbidden)
  x <- dplyr::arrange(x, ID, HLA) 
  
  return(x)
}

# create complexes - 




#grep("-", grep( "DQ|DP" , x$HLA, value = T), invert = T, value = T)


