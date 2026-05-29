#' funczion to translate the IDs form middle table into the required formats of commonly used prediction algorithms
#' 
#' @param alleles alleles to be translated
#' @param formats character cóvector, name of the algorithms, thats formats needs to be returned
#' @param hla_names data table to be used as reference
translateIDs <- function(alleles, formats , hla_names = NULL){
  # select chosen format columns
  req(is.character(formats), all(formats %in% c("netMHCpan", "MHCnuggets","mixMHCpred") ) )
  
  formats <- paste0("format_", formats)
  hla_names_sel <- hla_names |>
    dplyr::select(dplyr::all_of(c("HLA", formats)))
    
  # add translated formats to middle table 
  out <- hla_names_sel <- hla_names |>
    dplyr::select(dplyr::all_of(c("HLA", formats))) |>
    dplyr::filter(HLA %in% alleles)
  
  
  return(out)
}
