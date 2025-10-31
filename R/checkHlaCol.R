
accepted_hla_formats <- paste0(c("A", "B", "C", "DPA1", "DPB1", "DQA1", "DQB1", "DRB1"), "\\d\\d\\d\\d")

#
transformHlaFormat <- function(x) {
  
  # remove * and : from allele names
  if(any(grepl("\\*|:", x) ) ){
    message("Removing * and : from allele names")
    x <- gsub("\\*|:", "", x)
  } 
  
  #remove P from the end of the allele name
  if (any(grepl("P$|P-", x$HLA) ) ){
    message("Removing P from end of the allele names")
    x <- gsub("P$", "", x) 
    x <- gsub("P-", "-", x) 
  }
  
  # later alleles longer than 4 digits could be shortened
  
  return(x)
}

# function to check if a column is qualified to be a hla column
checkHlaCol <- function( col , delim = "," ) {
  # split cells by comma, if
  out <- strsplit(col, delim)
  
  # transform into accepted hla format - if possible
  out <- lapply(out, transformHlaFormat)
  
  # only keep values that fit the accepted hla_formats
  out <- lapply(out, function(x){
    grep(paste(accepted_hla_formats, collapse = "|"), x, value = T)
  })
  
  return(out)
}