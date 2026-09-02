

translateToNetMhcPan<- function(x){
  
  out <- character(length(x))
  
  where_drb <- substring(x, 1,3 ) == "DRB"
  
  out[where_drb] <- paste0(substring(x[where_drb], 1, 4), "_", substring(x[where_drb], 5) )
  out[!where_drb] <- paste0("HLA-", x[!where_drb])
  return(out) 
}


translateToMixMhcPred <- function(x){
  
  out <- x
  
  where_DR <- substring(x, 1, 2) == "DR"
  where_DQP <- substring(x, 1, 2) %in% c("DQ", "DP")
  
  out[where_DR] <- paste0(substring(x[where_DR], 1, 4), "_", 
                          substring(x[where_DR], 5, 6), "_", 
                          substring(x[where_DR], 7, 8) )
  
  out[where_DQP] <- paste0(substring(x[where_DQP], 1, 4), "_", 
                           substring(x[where_DQP], 5, 6), "_", 
                           substring(x[where_DQP], 7, 8), "__", 
                           substring(x[where_DQP], 10, 13), "_",
                           substring(x[where_DQP], 14, 15), "_",
                           substring(x[where_DQP], 16, 17)
                          )
  return(out)
}

translateToMhcNuggets <- function(x){
  
  where_DQP <- substring(x, 1, 2) %in% c("DQ", "DP")
  
  out <- paste0("HLA-", substring(x, 1, nchar(x)-2), ":", substring(x, nchar(x)-1))
  
  out[where_DQP] <- paste0(substring(out[where_DQP], 1, 10), ":", substring(out[where_DQP], 11))
  
  return(out)
}



test <- sample(unlist(allele_names), 50) |> unname()
translateToNetMhcPan(test)
translateToMixMhcPred(test)
translateToMhcNuggets(test)
