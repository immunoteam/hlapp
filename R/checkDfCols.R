
accepted_hla_formats <- paste0(c("A", "B", "C", "DPA1", "DPB1", "DQA1", "DQB1", "DRB1"), "\\d\\d\\d\\d")

#
# function to remove the 5th and 6th digit from an allele
trimId <- function(x, types = c("A", "B", "C", "DPB1", "DPA1", "DQA1", "DQB1", "DRB1")) {
  type_pattern <- paste0("(", paste(types, collapse = "|"), ")")
  
  if_pattern <-paste0(type_pattern, "(\\d{5})")
  pattern <- paste0(type_pattern, "(\\d{4})\\d{0,4}")
  sub_pattern <- "\\1\\2"
  
  
  if (any(grepl(if_pattern, x))) {
    message("Trimming allelle names to 4 digits")
    x <- gsub(pattern, sub_pattern, x)
  }
  return(x)
}


formatHlaFormat <- function(x, delim = ",") {
  
  # remove extra whitespace
  if(delim != " "){
    x <- gsub(" ", "", x) # remove spaces from rows
  } else {
    x <- gsub("  ", " ", x) # if delim is " " remove second spaces from rows
  }
  
  #  remove HLA or HLA- from the beginning of alleles names
  if (any(grepl("HLA|HLA-", x) ) ){
    message("Removing HLA from allele names")
    x <- gsub("HLA|HLA-", "", x) 
  }
  
  # remove * and : from allele names
  if(any(grepl("\\*|:|_", x) ) ){
    message("Removing * and : and _ from allele names")
    x <- gsub("\\*|:|_", "", x)
  } 
  
  #remove P from the end of the allele name
  P_pattern<- paste0("P$|P-|P", delim)
  if (any(grepl(P_pattern, x) ) ){
   message("Removing P from end of the allele names")
    x <- gsub("P$", "", x) 
    x <- gsub("P-", "-", x) 
    x <- gsub(paste0("P", delim), delim, x) 
  }
  
  # trim longer alleles to 4 digits
  x <- trimId(x)
  
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
