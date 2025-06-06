#' function copied from Anna's HLA_I_from middle_table.R code 
#' this function selects promiscuity values from the middle table
#' 
#' CHANGES FROM ORIGINAL
#' name of function was changed to camelCase for consistency
#' added middle_table as a function variable


selectPromiscuity <- function(middle_table, gene){
  if(startsWith(gene, "D")) n = 4 else n = 2
  prom <- matrix(NA, nrow = length(unique(middle_table$ID)), ncol = n)
  rownames(prom) <- unique(middle_table$ID)
  colnames(prom) <- paste0(gene, 1:n)
  for(id in unique(middle_table$ID)){
    table <- subset(middle_table, middle_table$ID == id & middle_table$Locus == gene)
    prom[id,] <- table$Promiscuity
    if(gene == "DQ"){
      prom[id, table$forbidden_DQ == "forbidden!"] <- "forbidden"
    }
  }
  return(prom)
}

