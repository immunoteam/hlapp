# data acces functions

read_allele <- function(allele){
  path <- file.path("data", "allele", 
                    paste0("allele_", allele, ".rds")
                    )
  
  readRDS(path)
}

read_cluster <- function(type, allele, cluster) {
  
  path <- file.path(
    "data",
    "cluster",
    paste0(
      "cluster_",
      gsub("HLA_", "", type),
      "_",
      allele,
      "_",
      cluster,
      ".rds"
    )
  )
  
  readRDS(path)
}

