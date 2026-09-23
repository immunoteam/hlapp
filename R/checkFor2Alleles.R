#check input - if one loci has only one value across all samples, then return a vwaning. 

# also return a warning for all calculated values, that would require two alleles to work. 

checkFor2Alleles <- function(middle_table){
  
  avail_loci <- unique(middle_table$Locus) # all loci that are in input
  
  max_alleles <- middle_table |> 
    dplyr::count(ID, Locus) |> 
    dplyr::group_by(Locus) |> 
    dplyr::summarise(max_Allele = max(n))
  
  
  warn_alleles <- max_alleles |>
    dplyr::filter(max_Allele < 2) |>
    dplyr::pull(Locus)

    return(warn_alleles)
}

# function to mutate column, if an allele is missing

mutateToWarning <- function(df, warn, col_to_mutate, col_check = "Locus"){
  
  if (col_check == "Class"){
   warn <- dplyr::recode_values(warn,
                         c("A", "B", "C") ~ "I", 
                         c("DP", "DQ", "DR") ~ "II")  |>  
     unique()
  }
  
  if ( length(warn) > 0) {
    df <- df |>
      mutate("{col_to_mutate}" := ifelse(.data[[col_check]] %in% warn, "Warning: Cannot be calculated", .data[[col_to_mutate]]) )
    
  }
  
  return(df)
}


checkForMoreAlleles <- function(middle_table){
  
  avail_loci <- unique(middle_table$Locus) # all loci that are in input
  
  max_alleles <- middle_table |> 
    dplyr::count(ID, Locus) |> 
    dplyr::group_by(Locus) |> 
    dplyr::summarise(max_Allele = max(n))
  
  
  warn_alleles <- max_alleles |>
    dplyr::filter(Locus %in% c("A", "B", "C", "DR") & max_Allele > 2 | Locus %in% c("DQ", "DP") & max_Allele > 4 ) |>
    dplyr::pull(Locus)
  
  return(warn_alleles)
}