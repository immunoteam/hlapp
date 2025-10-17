# functions to format input from cohorts and to calculate HLA-II promiscuity

#' formatting and filtering HLA promiscuity vector input for downstream use
#' 
#' @param x  the promiscuity vector. must be a named vector, where the names are HLA genotypes and the values are promiscuity values.
#' @param out.format whether to have the function output in a named vector or a dataframe (default). Recommended to use "df" as downstream code will require a data.frame
#' @param do.format whether to substitute "/" symbols to "-" in the HLA names.
#' @param do.filter whether to remove HLA genotypes that are uncertain (contain "or") or contain BoLA alleles
#' return a data.frame or a vector according to out.format
preparePromData<- function(x,  out.format = "df",
                                 do.format = T, do.filter = T){
  if (!out.format %in% c("df", "named.num") ){
    stop("Error: out.format must be either df, or named.num.")
  }
  
  if (do.format) {
    names(x) <- gsub("/", "-", names(x) ) 
  }
  if (do.filter){
    x <- x[grep("or|BoLA", names(x), invert = T ) ]
  }
  if (out.format == "df") {
    x <- data.frame(HLA = names(x),
                    Promiscuity = x) 
  } 
  return(x)
}
