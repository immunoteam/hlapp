#' copied from Anna's HLA_I_from middle_table.R code 
#' function to take the mean of selected promiscuity values
#' 
#' CHANGES FROM ORIGINAL
#' name of function was changed to camelCase for consistency
#' added middle_table as a function variable



meanPromiscuity <- function(middle_table, genes){
  prom <- rep(NA, length(unique(middle_table$ID)))
  names(prom) <- unique(middle_table$ID)
  for(id in unique(middle_table$ID)){
    table <- subset(middle_table, middle_table$ID == id & middle_table$Locus %in% genes)
    table <- subset(table, table$forbidden_DQ != "forbidden!")
    prom[id] <- mean(table$Promiscuity)
  }
  return(prom)
}
