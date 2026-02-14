#' funczion to translate the IDs form middle table into the required formats of commonly used prediction algorithms
#' 
#' @param middle_table middle_table
#' @param formats character cóvector, name of the algorithms, thats formats needs to be returned

translateIDs <- function( middle_table, hla_names, formats ){
  # select chosen format columns
  req(is.character(formats), all(formats %in% c("netMHCpan", "MHCnuggets","mixMHCpred") ) )
  formats <- paste0("format_", formats)
  hla_names_sel <- hla_names |>
    dplyr::select(dplyr::all_of(c("HLA", formats)))
    
  # add translated formats to middle table 
  out <- middle_table |>
    dplyr::select(dplyr::all_of(c("ID", "HLA") ) ) |>
    dplyr::left_join(hla_names_sel, by = dplyr::join_by(HLA))
  return(out)
}
