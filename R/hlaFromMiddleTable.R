#' Wrapper ffor selectPromiscuity and meanPromiscuity
#' essentially does tha same as HLA_I_from middle table, with minor canges
#'
#' CHANGES FROM ORIGINAL
#' added middle_table as a function variable

#' name of function was changed to camelCase for consistency

# new function should rely les son hard coding, - run all calculations within the tidyverse
# check if selected output columns are available. returna a warning message if not, but calculate available results
# the input side should be done step dy step, so an error is not even possible

#' function to create result table from middle table
#' @param middle_table middle_table calculated by the first part of the app
#' @param types user defined columns to include in results
hlaFromMiddleTable <- function(middle_table, types) {
  # check what values available from user input file - in a later point only these would be the option for user selection
  locus_allowed <- c("A", "B", "C", "DP", "DQ", "DR") # type of loci alloved
  locus_avail <- unique(middle_table$Locus) # loci found in  middle table
  locus_avail <- locus_avail[locus_avail %in% locus_allowed] #

  # get input options - individual and average
  ind_types <- grep("i$", types, value = T) # extract types with an i at the end- for individual output
  avg_types <- grep("a$", types, value = T) # extract types with an i at the end - for average outputs
  message(paste0("ind types: ", paste(ind_types, collapse = ", "), "\navg types: ", paste(avg_types, collapse = ", ")))

  # remove not allowed loci from middle table
  middle_table <- middle_table |>
    dplyr::filter(flag == "") |>
    dplyr::select(!any_of("flag"))


  # get individual promiscuity values - one row for each patient, one column for each
  prom_ind_all <- middle_table |>
    dplyr::group_by(ID, Locus) |>
    dplyr::mutate(Locus = paste(Locus, dplyr::row_number(), sep = "_")) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = all_of("Locus"), values_from = all_of(c("HLA", "Promiscuity")))

  # subset ind table to user selection
  if (length(ind_types) > 0) {
    prom_ind_sel <- prom_ind_all |>
      dplyr::select(any_of(c("ID")), contains(paste0("_", gsub("i", "_", ind_types))))
  } else {
    prom_ind_sel <- prom_ind_all |>
      dplyr::select(any_of(c("ID")))
  }

  prom_avg_all <- middle_table |>
    # remove flagged DQ alleles
    dplyr::group_by(ID, Locus) |>
    dplyr::summarise(mean_Promiscuity = mean(Promiscuity, na.rm = T), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Locus, values_from = mean_Promiscuity, names_prefix = "Promiscuity_mean_")

  if (length(avg_types) > 0) {
    prom_avg_sel <- prom_avg_all |>
      dplyr::select(any_of("ID"), contains(paste0("_", gsub("a", "", avg_types))))
  } else {
    prom_avg_sel <- prom_avg_all |>
      dplyr::select(any_of("ID"))
  }

  # table of selected individual and locus average promiscuities
  prom_sel <- dplyr::full_join(prom_ind_sel, prom_avg_sel, by = join_by(ID))

  # add class averages
  if ("class-Ia" %in% avg_types) {
    prom_sel$Promiscuity_mean_class_I <- round(rowMeans(dplyr::select(prom_avg_all, contains(c("_A", "_B", "_C"))), na.rm = TRUE), 4)
  }
  if ("class-IIa" %in% avg_types) {
    prom_sel$Promiscuity_mean_class_II <- round(rowMeans(dplyr::select(prom_avg_all, contains(c("_DP", "_DQ", "_DR"))), na.rm = TRUE), 4)
  }

  return(prom_sel) # use of prom_all is deprecatetd
}
