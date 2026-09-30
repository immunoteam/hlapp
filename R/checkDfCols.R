
accepted_hla_formats <- paste0(c("A", "B", "C", "DPA1", "DPB1", "DQA1", "DQB1", "DRB1"), "\\d\\d\\d\\d")

#
formatHlaFormat <- function(x) {
  
  # remove * and : from allele names
  if(any(grepl("\\*|:|_", x) ) ){
   # message("Removing * and : and _ from allele names")
    x <- gsub("\\*|:|_", "", x)
  } 
  
  #remove P from the end of the allele name
  if (any(grepl("P$|P-", x) ) ){
  #  message("Removing P from end of the allele names")
    x <- gsub("P$", "", x) 
    x <- gsub("P-", "-", x) 
  }
  
  # later alleles longer than 4 digits could be shortened
  
  return(x)
}

# function to check if a column is qualified to be a hla column
transformHlaCol <- function( col , delim = "," ) {
  # split cells by comma, if
  out <- strsplit(as.character(col), delim)
  
  # transform into accepted hla format - if possible
  out <- lapply(out, formatHlaFormat)
  
  # only keep values that fit the accepted hla_formats
  out <- lapply(out, function(x){
    grep(paste(accepted_hla_formats, collapse = "|"), x, value = T)
  })
  return(out)
}

checkHlaCol<- function(col, delim = ",") {
  recognised <-transformHlaCol(col, delim = delim) |>
    unlist() 
  
  length(recognised) > 0
}

# get the names of HLA columns
checkDfForHlaCols <- function(df) {
  res <- apply(head(df), 2, checkHlaCol)
  names(res[res])
}


# get the names of ID columns
checkIdCol <- function(col){
  length(col) == length(unique(col))
}

checkDfForIdCols <- function(df) {
  res <- apply(df, 2, checkIdCol)
  names(res[res])
}


### Combined function that takes one df and returns id and hla cols
checkDfCols <- function(df){
  
  # get potential columns
  hlacols <- checkDfForHlaCols(df)
  idcols <- checkDfForIdCols(df)
  
  # remove hla columns from id
  idcols <- idcols[!idcols %in% hlacols]
  
  return(list(idcols = idcols, hlacols = hlacols))
  
}
