#' copied from Anna's HLA_I_from middle_table.R code 
#' function to take the mean of selected promiscuity values
#' 
#' CHANGES FROM ORIGINAL
#' name of function was changed to camelCase for consistency
#' added middle_table as a function variable



meanPromiscuity <- function(middle_table, genes){
  
  genes <- gsub("a", "", genes) #remave a from the end of gene string
  
  prom <- matrix(NA, nrow = length(unique(middle_table$ID)), ncol = 1 )
  rownames(prom) <- unique(middle_table$ID)
  colnames(prom) <- paste0(paste(genes, collapse = ""), "_avg")
   
  for(id in unique(middle_table$ID)){
    table <- subset(middle_table, middle_table$ID == id & middle_table$Locus %in% genes)
    table <- subset(table, table$forbidden_DQ != "forbidden!")
    prom[id, ] <- mean(table$Promiscuity)
  }
  return(prom)
}
