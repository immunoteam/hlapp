#' Wrapper ffor selectPromiscuity and meanPromiscuity
#' essentially does tha same as HLA_I_from middle table, with minor canges
#' 
#' CHANGES FROM ORIGINAL
#' added middle_table as a function variable

#' name of function was changed to camelCase for consistency


#' function to create result table from middle table
#' @param middle_table middle_table calculated by the first part of the app
#' @param types user defined columns to include in results
hlaFromMiddleTable <- function(middle_table, types) {
  locus_allowed <- c("A", "B", "C", "DP", "DQ", "DR")
  
  locus_avail <- unique(middle_table$Locus)
  locus_avail <- locus_avail[locus_avail %in% locus_allowed]
  ind_all <- as.list(paste0(locus_avail, "i"))
  avg_all <- list("A", "B", "C", c("A", "B", "C"),
                  "DP", "DQ", "DR", c("DP", "DQ", "DR"))
  
  
  ind_types <- grep("i$", types, value = T) # extract types with an i at the end- for individual output
  avg_types <- grep("a$", types, value = T) # extract types with an i at the end - for average outputs
  message(paste0("ind types: ", paste(ind_types, collapse = ", "), "\navg types: ", paste(avg_types, collapse = ", ") ) )
  
  prom_ind <- lapply(ind_types, selectPromiscuity, middle_table= middle_table) %>%
    Reduce(cbind, .)
  message("prom int ok")
  
  prom_avg <- lapply(avg_types, meanPromiscuity, middle_table = middle_table) %>%
    Reduce(cbind, .)
  message("prom avg ok")
  
  prom_df <- cbind(prom_ind, prom_avg) #data frame with selected values
  message("prmdf ok")
  #prom_all <- prom_df
  
  message(paste0("ind types: ", paste(ind_all, collapse = ", "), "\navg types: ", paste(avg_all, collapse = ", ") ) )
  prom_all <- cbind( #data frame with all possible values
    Reduce(cbind, 
           lapply(ind_all, selectPromiscuity, middle_table = middle_table)
           ),
    Reduce(cbind, 
           lapply(avg_all, meanPromiscuity, middle_table = middle_table)
           )
    )
  message("promall ok")
  return(list(prom_df = prom_df, prom_all = prom_all))
}

# new function should rely les son hard coding, - run all calculations within the tidyverse
# check if selected output columns are available. returna a warning message if not, but calculate available results
  # the input side should be done step dy step, so an error is not even possible
hlaFromMiddleTable2 <- function(middle_table, types){
  
  # check what values available from user input file - in a later point only these would be the option for user selection
  locus_allowed <- c("A", "B", "C", "DP", "DQ", "DR") # type of loci alloved
  locus_avail <- unique(middle_table$Locus) # loci found in  middle table 
  locus_avail <- locus_avail[locus_avail %in% locus_allowed] # 
  
  # get input options - individual and average 
  ind_types <- grep("i$", types, value = T) # extract types with an i at the end- for individual output
  avg_types <- grep("a$", types, value = T) # extract types with an i at the end - for average outputs
  message(paste0("ind types: ", paste(ind_types, collapse = ", "), "\navg types: ", paste(avg_types, collapse = ", ") ) )
  
  #remove not alloowed loci from middle table
  
  # get individual promiscuity values - one row for each patient, one column for each 
  prom_ind_all <- middle_table |> 
    dplyr::group_by(ID, Locus) |>
    dplyr::mutate(Locus = paste(Locus, row_number(), sep = "_" ) ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = all_of("Locus"), values_from = all_of(c("HLA", "Promiscuity")))
  
  # subset ind table to user selection  
  prom_ind_sel <- prom_ind_all |> 
    select(any_of(c("ID")), contains( paste0("_", gsub("i", "_", ind_types) ) ) )
  
  prom_avg_all <-  middle_table |>
    # remove flagged DQ alleles
    dplyr::group_by(ID, Locus) |>
    dplyr::summarise(mean_Promiscuity = mean(Promiscuity, na.rm = T), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Locus, values_from = mean_Promiscuity, names_prefix = "Promiscuity_mean_")
  
  prom_avg_sel <- prom_avg_all |>
    select(any_of("ID"), contains( paste0("_", gsub("a", "", avg_types) ) ) )
  
  prom_sel <- full_join(prom_ind_sel, prom_avg_sel, by = join_by(ID))
  prom_all <- full_join(prom_ind_all, prom_avg_all, by = join_by(ID))
  
  return(list(prom_sel = prom_sel, prom_all = prom_all))
}
