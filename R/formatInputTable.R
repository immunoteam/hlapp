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

formatCohortTable <- function(x, delim = ",") {
  select_cols <- c("ID", "HLA_I", "HLA_II")
  
  x |>
    dplyr::select(tidyr::all_of(select_cols) ) |>
    dplyr::mutate(HLA = paste(HLA_I, HLA_II, sep = ",")) |>
    tidyr::separate_longer_delim(cols = tidyr::all_of("HLA"), delim = delim ) |>
    dplyr::select(ID, HLA)
}

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
    x$forbidden_DQ <- ifelse(x$AB_comb %in% forbidden_DQ_combinations, "forbidden!", "") 
  } else {
    x <-  dplyr::filter(x, !AB_comb %in% forbidden_DQ_combinations)
    }
  
  x$AB_comb <- NULL
  
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
  
  #rm forbidden alpha-beta combinations

  x <- rmForbiddenCombinations(x, flag = flag.forbidden)
  x <- dplyr::arrange(x, ID, HLA)
  
  return(x)
}
 

formatManualTable <- function(x, id.col, hla.cols, delim = ","){
  x <- x |>
    dplyr::select(all_of(c(id.col, hla.cols))) |>
    dplyr::rename(ID = .data[[id.col]]) |>
    tidyr::unite(HLA, all_of(hla.cols), sep = delim) |>
    tidyr::separate_longer_delim(HLA, delim = delim)
  return(x)
}
