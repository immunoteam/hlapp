# functions to format input from cohorts and to calculate HLA-II promiscuity

#' formatting and filtering HLA promiscuity vector input for downstream use
#' 
#' @param x  the promiscuity vector. must be a named vector, where the names are HLA genotypes and the values are promiscuity values.
#' @param out.format whether to have the function output in a named vector or a dataframe (default). Recommended to use "df" as downstream code will require a data.frame
#' @param do.format whether to substitute "/" symbols to "-" in the HLA names.
#' @param do.filter whether to remove HLA genotypes that are uncertain (contain "or") or contain BoLA alleles
#' return a data.frame or a vector according to out.format
formFilterPromVector <- function(x, 
                                 out.format = "df",
                                 hla.col= "HLA_II",
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
    colnames(x) <- c(hla.col, "Promiscuity")
  } 
  return(x)
}

#'remove forbidden HLA-DQ combinations from datasets
#'
#'@param x dataframe containing acolumn of HLA molecule genotypes
#'@param hla.col name of the column containig hla - genotype
#'
#'@return a filtered data.frame, with the same coluns as x
rmForbiddenCombinations <- function(x, 
                                    hla.col = "HLA_II"){
  require(dplyr)
  forbidden_combinations <- c("DQA101-DQB102", "DQA101-DQB103",
                              "DQA103-DQB105", "DQA103-DQB106",
                              "DQA105-DQB105", "DQA105-DQB106", 
                              "DQA103-DQB102", "DQA104-DQB105" # copied in form the data/forbidden_dq.csv file provided by Gergő Balogh
  )
  
  x$TMP <- x[ , hla.col]
  x <- x |> 
    mutate(AB_comb  = paste(substr(TMP, 1, 6), 
                            substr(TMP, 10, 15), 
                            sep = "-" )) |>
    filter(!AB_comb %in% forbidden_combinations) |>
    select(!all_of(c("TMP", "AB_comb" ) ) )
  
  return(x)
}

#' format patient HLA genotype into a optimal format for promiscuity calculation
#' 
#' @param x the data frame containing patient data. should contain select.cols
#' @param select.cols a char. vector of two. the first element should be the name of the column containing patient ID, and the second element should be the name of the column that contains the HLA genotype of interest.
#' @param delim character value to separate the individual HLA alleles by
#' @param rm.forbidden wether to remove forbidden HLAQ alpha-beta combinations for the table. TRUE by default.
#' 
#' @return a data frame
formatCohortTable <- function(x, 
                              select.cols = c("ID","HLA_II"),
                              delim = ",",
                              rm.forbidden = T){
  require(dplyr)
  #format df
  x <- x |>
    select(all_of(select.cols) ) |>
    separate_longer_delim(cols = all_of(select.cols[2] ), delim = delim )
  #rm forbidden alpha-beta combinations
  if (rm.forbidden) {
    x <- rmForbiddenCombinations(x, hla.col = select.cols[2])
  }
  
  return(x)
}


#' function to join the HLA genotype and promiscuity
#' @param x data frame of patient HLA genotype. 
#'@param y data frame containig HLA promiscuity values. Output of the fromFilterPromVector function
#'@param hla.col column of x. Contains HLA genotype
joinGenotypePromiscuity <- function(x, y, hla.col = "HLA_II") {
  str.end <- ifelse(hla.col == "HLA_I", 1, 2)
  x$Allele <- sub("[^a-zA-Z]", "", x[, hla.col]) |> substr(1, str.end)
  
  res <- left_join(x, y)
  
  return(res)
}
  
#'calculate the allele and genotype promiscuity for the patients of a cohort
#'
#'@param x the data frame containing the HLA genotypes and promiscuity values of patients, output of joinGenotypePromiscuity
calcGenotypePromiscuity <- function(x){
  require(dplyr) 
  
  res <- x |>
    group_by(ID, Allele) |>
    summarise(Promiscuity = mean(Promiscuity, na.rm = F), .groups = "drop") |>
    pivot_wider(names_from = Allele, values_from = Promiscuity, names_prefix = "Prom_")
  res$Prom_GT = rowMeans(select(res, contains("Prom_")), na.rm = F)
  
  return(res)
}

#' function to respond to if file input is uploaded/received
#' 
#' @param file input$file 
textResponseToInput <- function(file){
  if (is.null(file)) { 
    out <- "No file input, detected. Text input is used detected"
  } 
  else  {
    out <- "File input detected"
  } 
  return(out)
}


