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
